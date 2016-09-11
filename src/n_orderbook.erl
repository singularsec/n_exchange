-module(n_orderbook).

-ifdef(EUNIT).
-compile(export_all).
-else.
-export([create/1, add_new_order_single/2, dump/1, qa_fillbook/1, try_cancel_order/2, try_change_order/2]).
-endif.

-include("../include/secexchange.hrl").

-record(execplan, {selected=[], price=0, qtdleft}).

% http://www.fixdeveloper.com/2014/05/fix-44-order-state-change-matrices.html

create(Symbol) ->
  BuyTName  = list_to_atom( atom_to_list(buy)  ++ "_" ++ Symbol ),
  SellTName = list_to_atom( atom_to_list(sell) ++ "_" ++ Symbol ),
  BuysT  = ets:new(BuyTName,  [ordered_set, {keypos, #order.oid}]),
  SellsT = ets:new(SellTName, [ordered_set, {keypos, #order.oid}]),
  #orderbook{sells=SellsT, buys=BuysT, lasttrade=0, symbol=Symbol}.


try_change_order(#order_modify{side=buy} = Order, #orderbook{buys=BuysT} = Book) ->
  ClOrderId = Order#order_modify.orig_cl_ord_id,
  MatcherByClOrdId = fun (Item) -> if Item#order.cl_ord_id =:= ClOrderId -> Item; true -> nil end end,
  MatchedBuy = find_in_ets_table(BuysT, nil, MatcherByClOrdId),
  if 
    MatchedBuy =:= nil -> not_found;
    true -> 
      modify_order(MatchedBuy, Order, Book), 
      done
  end;

try_change_order(#order_modify{side=sell} = Order, #orderbook{sells=SellsT} = Book) ->
  ClOrderId = Order#order_modify.orig_cl_ord_id,
  MatcherByClOrdId = fun (Item) -> if Item#order.cl_ord_id =:= ClOrderId -> Item; true -> nil end end,
  MatchedSell = find_in_ets_table(SellsT, nil, MatcherByClOrdId),
  if 
    MatchedSell =:= nil -> not_found;
    true -> 
      modify_order(MatchedSell, Order, Book), 
      done
  end.


try_cancel_order(#order_cancel{side=buy} = CancelOrder, #orderbook{buys=BuysT} = Book) ->
  ClOrderId = CancelOrder#order_cancel.orig_cl_ord_id,
  MatcherByClOrdId = fun (Item) -> if Item#order.cl_ord_id =:= ClOrderId -> Item; true -> nil end end,
  MatchedBuy = find_in_ets_table(BuysT, nil, MatcherByClOrdId),
  if 
    MatchedBuy =:= nil -> not_found;
    true -> 
      cancel_order(MatchedBuy, undefined, Book), 
      done
  end;

try_cancel_order(#order_cancel{side=sell} = CancelOrder, #orderbook{sells=SellsT} = Book) ->
  ClOrderId = CancelOrder#order_cancel.orig_cl_ord_id,
  MatcherByClOrdId = fun (Item) -> if Item#order.cl_ord_id =:= ClOrderId -> Item; true -> nil end end,
  MatchedSell = find_in_ets_table(SellsT, nil, MatcherByClOrdId),
  if 
    MatchedSell =:= nil -> not_found;
    true -> 
      cancel_order(MatchedSell, undefined, Book),
      done
  end.


dump(#orderbook{buys=BuysT, sells=SellsT, symbol=S}) ->
  Collector = fun (Item, Acc) -> [pretty_print_order(Item)] ++ Acc end,
  Buys  = traverse_ets_table(BuysT,  nil, Collector, []),
  Sells = traverse_ets_table(SellsT, nil, Collector, []),
  {S, Buys, Sells}.

qa_fill_item([#order{} = Order| Rest], Book) ->
  NewMatchingOrder = #order{
        symbol     = Order#order.symbol,
        cl_ord_id  = "cl1",
        order_qty  = Order#order.qtd_left,
        qtd_left   = Order#order.qtd_left,
        price      = Order#order.price / 10000,
        price_type = Order#order.price_type,
        order_status = new,
        order_type = limit,
        side       = opposite_side(Order#order.side),
        account    = "acc1",
        from_sessionid = "CCLR", to_sessionid="SOME"
  },
  add_new_order_single(NewMatchingOrder, Book),
  qa_fill_item(Rest, Book);

qa_fill_item([], Book) ->  ok.

% matches all open orders with a counterpart
qa_fillbook(#orderbook{buys=BuysT, sells=SellsT} = Book) ->
  Collect = fun (Item, Acc) -> [Item] ++ Acc end,
  Buys  = traverse_ets_table(BuysT,  nil, Collect, []),
  Sells = traverse_ets_table(SellsT, nil, Collect, []),
  ok = qa_fill_item(Buys, Book),
  ok = qa_fill_item(Sells, Book),
  dump(Book).

add_new_order_single(#order{price=Price,side=Side,order_qty=Qtd} = Order, Book) ->
  NormalizedPrice = normalize_price(Price),
  if 
    NormalizedPrice =:= 1230000 ->
      % magic number causes a rejection!
      {Key, Time} = compose_key(Price, Side),
      UniqueId = erlang:unique_integer([positive]),
      NewOrder = Order#order{oid=Key,
                             id=integer_to_list(UniqueId),
                             time=Time,
                             order_status=canceled,
                             order_qty=Qtd, qtd_filled=0, qtd_left=Qtd, qtd_last=0,
                             price=NormalizedPrice},
      send_reject_notification(NewOrder, "perdeu prayboy");

    true -> 
      SupportedOrderTypes = [ limit, market, stop, stoplimit, marketwithleftoverlimit ],
      IsValid = lists:member(Order#order.order_type, SupportedOrderTypes),
      add_new_order_single(Order, Book, IsValid)
  end.

add_new_order_single(#order{} = Order, _Book, false) ->
  Msg = "Unsupported order type: " ++ atom_to_list(Order#order.order_type),
  send_reject_notification(Order, Msg);

add_new_order_single(#order{} = Order, Book, true) ->
  NewOrder = insert_order(Order, Book),
  NewOrder.

-spec insert_order('order', 'orderbook') -> 'order' | ok.
insert_order(#order{side=buy} = Order, Book) ->
  Table = Book#orderbook.buys,
  NewOrder = insert_order_into(Order, Table),
  match_new_order(NewOrder, Book);

insert_order(#order{side=sell} = Order, Book) ->
  Table = Book#orderbook.sells,
  NewOrder = insert_order_into(Order, Table),
  match_new_order(NewOrder, Book);

insert_order(#order{side=_OtherSide} = Order, Book) ->
  send_reject_notification(Order, "Unsupported side: " ++ atom_to_list(_OtherSide) ),
  false.

insert_order_into(#order{price=Price, order_qty=Qtd, side=Side} = Order, Table) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = compose_key(Price, Side),
  UniqueId = erlang:unique_integer([positive]),
  NewOrder = Order#order{oid=Key,
                         id=integer_to_list(UniqueId),
                         time=Time,
                         order_status=new,
                         order_qty=Qtd, qtd_filled=0, qtd_left=Qtd, qtd_last=0,
                         price=NormalizedPrice},
  ets:insert(Table, NewOrder),
  send_accept_notification(NewOrder),
  NewOrder.

% nothing to do
match_new_order(false, _Book) -> ok;

match_new_order(#order{side=Side,order_qty=Qtd,order_type=market,timeinforce=TiF} = Order, Book) ->
  OtherSideT = other_side(Side, Book),
  Plan = create_matching_plan(OtherSideT, Side, 0, Qtd),
  execute_plan_applying_timeinforce(Order, Plan, TiF, Book);

match_new_order(#order{side=Side,order_qty=Qtd,price=Price,order_type=limit,timeinforce=TiF} = Order, Book) ->
  OtherSideT = other_side(Side, Book),
  Plan = create_matching_plan(OtherSideT, Side, Price, Qtd),
  execute_plan_applying_timeinforce(Order, Plan, TiF, Book);

match_new_order(#order{side=Side,order_qty=Qtd,price=Price,order_type=stop,timeinforce=TiF} = Order, Book) ->
  error(unimplemented);

match_new_order(#order{side=Side,order_qty=Qtd,price=Price,order_type=stoplimit,timeinforce=TiF} = Order, Book) ->
  error(unimplemented);

match_new_order(#order{side=Side,order_qty=Qtd,price=Price,order_type=marketwithleftoverlimit,timeinforce=TiF} = Order, Book) ->
  error(unimplemented).

% traverse table of available orders selecting compatible orders.
create_matching_plan(OrderT, Side, Price, Qtd) ->
  Collector =
    fun (Item, State) ->
      #execplan{selected=Selected, qtdleft=QtdToFill, price=DesiredPrice}=State,
      if
        QtdToFill =< 0 -> done;
        true ->
          case apply_selection_criterion(Side, Item, DesiredPrice) of
            true ->
              NewQtd = QtdToFill - min(Item#order.qtd_left, QtdToFill),
              State#execplan{selected=Selected ++ [Item], qtdleft=NewQtd};
            false -> State
          end
      end
  end,
  traverse_ets_table(OrderT, nil, Collector, #execplan{selected=[], price=Price, qtdleft=Qtd}).

apply_selection_criterion(buy, #order{price=Price, order_type=Type}, DesiredPrice)->
  Price =< DesiredPrice;
apply_selection_criterion(sell, #order{price=Price, order_type=Type}, DesiredPrice)->
  Price >= DesiredPrice.

% Fill Or Kill (FOK/All_or_nothing) orders (59 = 4) require that the full amount stated in the order is
% executed upon entering the order book. If there is not enough quantity on the opposite
% side to fill the order, the order is acknowledged then cancelled.
execute_plan_applying_timeinforce(Order,
                                  #execplan{selected=Orders, qtdleft=LeavesQtd} = Plan,
                                  fillorkill, Book) ->
  case LeavesQtd > 0 of
    true  -> cancel_order(Order, "not enough quantity on the opposite side", Book);
    false -> execute_plan(Order, Plan, Book)
  end;

% The Immediate or Cancel validity (59 = 3), also known as Fill and Kill (FAK),
% indicates that the order requires immediate execution, and the unexecuted quantity is
% automatically cancelled. If there is no counterparty to execute against,
% the order is acknowledged then cancelled
execute_plan_applying_timeinforce(Order,
                                  #execplan{selected=[]},
                                  immediateorcancel, Book) ->
  cancel_order(Order, "not counterparty on the opposite side", Book);

execute_plan_applying_timeinforce(Order,
                                  #execplan{selected=Orders, qtdleft=LeavesQtd} = Plan,
                                  immediateorcancel, Book) ->
  NewOrder = execute_plan(Order, Plan, Book),
  case LeavesQtd > 0 of
    true  -> cancel_order(NewOrder, "unexecuted quantity cancelled", Book);
    _ -> NewOrder
  end;

execute_plan_applying_timeinforce(Order,
                                  #execplan{selected=[]},
                                  _TimeInForce, _Book) ->
  % nothing matched, so nothing else to do
  Order;

execute_plan_applying_timeinforce(Order,
                                  #execplan{qtdleft=LeavesQtd} = Plan,
                                  TimeInForce, Book) ->
  % day | goodtillcancel | goodtilldate | attheclose | goodforauction
  NewOrder = execute_plan(Order, Plan, Book),
  NewOrder.

execute_plan(Order, [], _Book) ->
  Order;

execute_plan(Order, #execplan{selected=Orders}, Book) ->
  execute_plan(Order, Orders, Book);

execute_plan(#order{qtd_filled=Filled,qtd_left=LeavesQtd}=Order, [MatchedOrder|Rest], Book) ->
  #order{qtd_left=AvailableQtd, price=MatchPrice} = MatchedOrder,

  HowMany = min(LeavesQtd, AvailableQtd),

  NewMatchedOrder = decrement_qtd_and_record(HowMany, MatchedOrder, Order, MatchPrice),
  NewOrder        = decrement_qtd_and_record(HowMany, Order, MatchedOrder, MatchPrice),

  record_match(NewMatchedOrder, Book),
  record_match(NewOrder, Book),

  notify_trade(NewMatchedOrder, NewOrder, HowMany, MatchPrice),

  execute_plan(NewOrder, Rest, Book).


decrement_qtd_and_record(ByHowMany,
                         #order{order_qty=Original, qtd_filled=Filled, qtd_left=LeavesQtd, qtd_last=Last, matches=MList}=Order,
                         #order{id=OtherId} = _WithOrder, Price) ->
  % TODO: use Original to assert consistency
  NewStatus =
    case Filled + ByHowMany of
      Original -> filled;
      _ -> partially_filled % fix 44: partial
    end,
  NewList = [{Price,ByHowMany,OtherId}] ++ MList,
  Order#order{order_status=NewStatus,
              qtd_filled=Filled + ByHowMany,
              qtd_left=LeavesQtd - ByHowMany,
              qtd_last=ByHowMany,
              matches = NewList}.

% ----- Changes order state + the ets tables

record_match(#order{qtd_left=0} = Order, Book) ->
  complete_order(Order, Book);

record_match(Order, Book) ->
  partial_fill_order(Order, Book).

partial_fill_order(Order, Book) ->
  replace_in_ets(Order, Book),
  send_partial_fill_notification(Order),
  Order.

complete_order(#order{oid=Key, side=Side} = Order, Book) ->
  remove_from_ets(Order, Book),
  send_filled_notification(Order),
  Order.

reject_order(Order, Reason, Book) ->
  NewOrder = Order#order{order_status=rejected},
  remove_from_ets(NewOrder, Book),
  send_reject_notification(NewOrder, Reason),
  NewOrder.

cancel_order(Order, Reason, Book) ->
  NewOrder = Order#order{order_status=canceled},
  remove_from_ets(NewOrder, Book),
  send_cancel_notification(NewOrder, Reason),
  NewOrder.

modify_order(#order{side=Side} = ExistingOrder, 
             #order_modify{cl_ord_id=ClOrdId, price=NewPrice}, 
             Book) -> 
  Table = get_table(Side, Book),
  NormalizedPrice = normalize_price(NewPrice),
  {Key, Time} = compose_key(NewPrice, Side),
  UniqueId = erlang:unique_integer([positive]),
  NewOrder = ExistingOrder#order{oid=Key,
                                 time=Time,
                                 id=integer_to_list(UniqueId),
                                 price=NormalizedPrice, 
                                 % qtd=NewQtd, 
                                 cl_ord_id=ClOrdId,
                                 orig_cl_ord_id=ExistingOrder#order.cl_ord_id},
  remove_from_ets(ExistingOrder, Book),
  ets:insert(Table, NewOrder),
  send_replace_notification(NewOrder),
  match_new_order(NewOrder, Book),
  NewOrder.

% ----- ets tables

replace_in_ets(#order{oid=Key, side=Side} = Order, Book) ->
  true = ets:insert(get_table(Side, Book), Order).

remove_from_ets(#order{oid=Key, side=Side} = Order, Book) ->
  true = ets:delete(get_table(Side, Book), Key).

% ----- Events that generate Execution reports

send_accept_notification(Order) ->
    nexchange_trading_book_eventmgr:notify_accept(Order).

send_reject_notification(Order, Reason) ->
    nexchange_trading_book_eventmgr:notify_rejection(Order, Reason).

send_cancel_notification(Order, Reason) ->
    nexchange_trading_book_eventmgr:notify_cancel(Order, Reason).

send_filled_notification(Order) ->
    nexchange_trading_book_eventmgr:notify_fill(Order).

send_partial_fill_notification(Order) ->
    nexchange_trading_book_eventmgr:notify_partial_fill(Order).

send_replace_notification(Order) ->
  nexchange_trading_book_eventmgr:notify_replace(Order).

notify_trade(MatchedOrder = #order{id=RefId, symbol=Symbol, from_sessionid=To},
             Order = #order{from_sessionid=From},
             HowMany, MatchPrice) ->

    Trade = #tradeinfo{symbol=Symbol,
                       refid=RefId,
                       price=MatchPrice,
                       qtd=HowMany,
                       buyer=From,
                       seller=To},

    nexchange_trading_book_eventmgr:notify_trade(Trade).

% ----- Utility functions

normalize_price(Price) when is_atom(Price) -> 0;
normalize_price(Price) when is_number(Price) ->
    % remove 4 decimals by multipling by 10000
    round(Price * 10000). % round converts it back to integer

traverse_ets_table(Table, Key, Collector, State) ->
  KeyToUse = case Key of
    nil ->  ets:first(Table);
    _ ->    ets:next(Table, Key)
  end,
  case KeyToUse of
    '$end_of_table' -> State; % end of table, return state
    _ ->                      % evaluate next line
      [Line|_] = ets:lookup(Table, KeyToUse), % this always returns a list
      NewState = Collector(Line, State),
      if
        % if Collector returns 'done' we stop traversing and return previous state
        NewState =:= done -> State;
        true -> traverse_ets_table(Table, KeyToUse, Collector, NewState)
      end
  end.

% If predicate returns something other than the LINE we keep moving, otherwise returns Line (Row)
find_in_ets_table(Table, Key, Predicate) ->
  KeyToUse = case Key of
    nil ->  ets:first(Table);
    _ ->    ets:next(Table, Key)
  end,
  case KeyToUse of
    '$end_of_table' -> nil; % end of table, return state
    _ ->                      % evaluate next line
      [Line|_] = ets:lookup(Table, KeyToUse), % this always returns a list
      NewState = Predicate(Line),
      if
        % if Predicate returns the Line we stop traversing and return Line
        NewState =:= Line -> Line;
        true -> find_in_ets_table(Table, KeyToUse, Predicate)
      end
  end.

get_table(buy,  #orderbook{buys=BuysT})   -> BuysT;
get_table(sell, #orderbook{sells=SellsT}) -> SellsT.

other_side(buy, #orderbook{sells=SellsT}) -> SellsT;
other_side(sell, #orderbook{buys=BuysT}) -> BuysT.

opposite_side(buy) -> sell;
opposite_side(sell) -> buy.

get_now() -> erlang:system_time(micro_seconds). % - 1441736774862944.

compose_key(Price, sell) ->
  Now = get_now(),
  Res = (Price * 100000000000000000) + Now,
  {Res, Now};

compose_key(Price, buy) ->
  Now = get_now(),
  Res = (Price * -100000000000000000) + Now,
  {Res, Now}.

pretty_print_order(#order{qtd_filled=Filled,
          qtd_left=Left,
          qtd_last=Last,
          price=Price,
          order_status=St,
          order_type=Ty,
          side=Sd,
          account=Ac,
          id=Id} = Order) ->

    [{id, Id}, {side, Sd}, {price, Price/10000},
    {type, Ty}, {status, St}, {acc, Ac}, {qtd, Filled, Left, Last}].














%
