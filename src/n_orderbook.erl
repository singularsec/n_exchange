-module(n_orderbook).

-ifdef(EUNIT).
-compile(export_all).
-else.
-export([create/1, add_new_order_single/2]).
-endif.

-include("../include/secexchange.hrl").

-record(execplan, {selected=[], price=0, qtdleft}).

% http://www.fixdeveloper.com/2014/05/fix-44-order-state-change-matrices.html

create(Symbol) ->
  BuyTName  = list_to_atom( atom_to_list(buy)  ++ "_" ++ Symbol ),
  SellTName = list_to_atom( atom_to_list(sell) ++ "_" ++ Symbol ),
  BuysT  = ets:new(BuyTName,  [ordered_set, {keypos, #order.oid}]),
  SellsT = ets:new(SellTName, [ordered_set, {keypos, #order.oid}]),
  #orderbook{sells=SellsT, buys=BuysT, lasttrade=0}.

add_new_order_single(#order{} = Order, Book) ->
  SupportedOrderTypes = [ limit, market, stop, stoplimit, marketwithleftoverlimit ],
  IsValid = lists:member(Order#order.order_type, SupportedOrderTypes),
  add_new_order_single(Order, Book, IsValid).

add_new_order_single(#order{} = Order, _Book, false) ->
  send_reject_notification(Order, "Unsupported order type: " ++ atom_to_list(Order#order.order_type) );

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

insert_order_into(#order{price=Price, qtd=Qtd, side=Side} = Order, Table) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = compose_key(Price, Side),
  NewOrder = Order#order{oid=Key,
                         time=Time,
                         order_status=new,
                         qtd_filled=0, qtd_left=Qtd, qtd_last=0,
                         price=NormalizedPrice},
  ets:insert(Table, NewOrder),
  send_accept_notification(NewOrder),
  NewOrder.

% nothing to do
match_new_order(false, _Book) -> ok;

match_new_order(#order{side=Side,qtd=Qtd,order_type=market,timeinforce=TiF} = Order, Book) ->
  OtherSideT = other_side(Side, Book),
  Plan = create_matching_plan(OtherSideT, Side, 0, Qtd),
  execute_plan_applying_timeinforce(Order, Plan, TiF, Book);

match_new_order(#order{side=Side,qtd=Qtd,price=Price,order_type=limit,timeinforce=TiF} = Order, Book) ->
  OtherSideT = other_side(Side, Book),
  Plan = create_matching_plan(OtherSideT, Side, Price, Qtd),
  execute_plan_applying_timeinforce(Order, Plan, TiF, Book);

match_new_order(#order{side=Side,qtd=Qtd,price=Price,order_type=stop,timeinforce=TiF} = Order, Book) ->
  error(unimplemented);

match_new_order(#order{side=Side,qtd=Qtd,price=Price,order_type=stoplimit,timeinforce=TiF} = Order, Book) ->
  error(unimplemented);

match_new_order(#order{side=Side,qtd=Qtd,price=Price,order_type=marketwithleftoverlimit,timeinforce=TiF} = Order, Book) ->
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
    false -> complete_order(NewOrder, Book)
  end;

execute_plan_applying_timeinforce(Order,
                                  #execplan{selected=Orders, qtdleft=LeavesQtd} = Plan,
                                  TimeInForce, Book) ->
  % day | goodtillcancel | goodtilldate | attheclose | goodforauction
  NewOrder = execute_plan(Order, Plan, Book),
  case LeavesQtd > 0 of
    true  -> partial_fill_order(NewOrder, Book);
    false -> complete_order(NewOrder, Book)
  end.

execute_plan(Order, #execplan{selected=Orders}, Book) ->
  execute_plan(Order, Orders, Book);

execute_plan(Order, [], _Book) ->
  Order;

execute_plan(#order{qtd_filled=Filled,qtd_left=LeavesQtd}=Order, [MatchedOrder|Rest], Book) ->
  #order{qtd_left=AvailableQtd} = MatchedOrder,

  HowMany = min(LeavesQtd, AvailableQtd),

  NewMatchedOrder = decrement_qtd(HowMany, MatchedOrder),
  NewOrder        = decrement_qtd(HowMany, Order),

  update_state(NewMatchedOrder, Book),
  update_state(NewOrder, Book),

  execute_plan(Order, Rest, Book).

% qtd, qtd_filled, qtd_left, qtd_last
decrement_qtd(ByHowMany, #order{qtd=_Original, qtd_filled=Filled, qtd_left=LeavesQtd, qtd_last=Last}=Order) ->
  % TODO: use Original to assert consistency
  Order#order{qtd_filled=Filled + ByHowMany, qtd_left=LeavesQtd - ByHowMany, qtd_last=ByHowMany}.


% ----- Changes order state + the ets tables

update_state(#order{qtd_left=0} = Order, Book) ->
  complete_order(Order, Book);

update_state(Order, Book) ->
  partial_fill_order(Order, Book).

partial_fill_order(Order, Book) ->
  replace_in_ets(Order, Book),
  send_partial_fill_notification(Order),
  Order.

complete_order(#order{id=Key, side=Side} = Order, Book) ->
  remove_from_ets(Order, Book),
  send_filled_notification(Order),
  Order.

reject_order(Order, Reason, Book) ->
  remove_from_ets(Order, Book),
  send_reject_notification(Order, Reason),
  Order.

cancel_order(Order, Reason, Book) ->
  remove_from_ets(Order, Book),
  send_cancel_notification(Order, Reason),
  Order.

% ----- ets tables

replace_in_ets(#order{id=Key, side=Side} = Order, Book) ->
  true = ets:insert(get_table(Side, Book), Order).

remove_from_ets(#order{id=Key, side=Side} = Order, Book) ->
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

get_table(buy,  #orderbook{buys=BuysT})   -> BuysT;
get_table(sell, #orderbook{sells=SellsT}) -> SellsT.

other_side(buy, #orderbook{sells=SellsT}) -> SellsT;
other_side(sell, #orderbook{buys=BuysT}) -> BuysT.

get_now() -> erlang:system_time(micro_seconds). % - 1441736774862944.

compose_key(Price, sell) ->
  Now = get_now(),
  Res = (Price * 100000000000000000) + Now,
  {Res, Now};

compose_key(Price, buy) ->
  Now = get_now(),
  Res = (Price * -100000000000000000) + Now,
  {Res, Now}.















%
