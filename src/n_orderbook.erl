-module(n_orderbook).

-ifdef(EUNIT).
-compile(export_all).
-else.
-export([create/1, add_new_order_single/2]).
-endif.

-include("../include/secexchange.hrl").

-record(execplan, {selected=[], price=0, qtdleft}).


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
  send_reject(Order, "Unsupported order type: " ++ atom_to_list(Order#order.order_type) );

add_new_order_single(#order{} = Order, Book, true) ->
  NewOrder = insert_order(Order, Book),

  % day goodtillcancel
  % immediateorcancel
  % fillorkill
  % goodtilldate
  % attheclose
  % goodforauction

  ok.

insert_order(#order{side=buy} = Order, Book) ->
  Table = Book#orderbook.buys,
  NewOrder = insert_order_into(Order, Table),
  match_new_order(NewOrder, Book);

insert_order(#order{side=sell} = Order, Book) ->
  Table = Book#orderbook.sells,
  NewOrder = insert_order_into(Order, Table),
  match_new_order(NewOrder, Book);

insert_order(#order{side=_OtherSide} = Order, Book) ->
  send_reject(Order, "Unsupported order side: " ++ atom_to_list(_OtherSide) ),
  false.

insert_order_into(#order{price=Price, qtd=Qtd, side=Side} = Order, Table) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = compose_key(Price, Side),
  NewOrder = Order#order{oid=Key,
                         time=Time,
                         qtd_filled=0, qtd_left=Qtd, qtd_last=0,
                         price=NormalizedPrice},
  ets:insert(Table, NewOrder),
  send_accept(NewOrder),
  NewOrder.


% nothing to do
match_new_order(false, _Book) -> ok;

match_new_order(#order{side=Side,qtd=Qtd,order_type=market,timeinforce=TiF} = Order, Book) ->
  OtherSideT = other_side(Side, Book),
  Plan = create_matching_plan(OtherSideT, Side, 0, Qtd),
  execute_plan_if_compatible_with_timeinforce(Plan);

match_new_order(#order{side=Side,order_type=limit,timeinforce=TiF} = Order, Book) ->
  ok.

% traverse table of available orders selecting compatible orders.
create_matching_plan(OrderT, Side, Price, QtdToFill) ->
  Collector = fun (Item, #execplan{selected=Selected, qtdleft=QtdToFill, price=DesiredPrice} ) ->
    if
      QtdToFill =< 0 -> done;
      true ->
        if
          Item#order.price >= DesiredPrice ->
            NewQtd = QtdToFill - min(Item#order.qtd, QtdToFill),
            {Selected ++ [Item], NewQtd, DesiredPrice};
          true ->
            {Selected, QtdToFill, DesiredPrice}
        end
    end
  end,
  traverse_ets_table(OrderT, nil, Collector, #execplan{price=Price, qtdleft=QtdToFill}).

execute_plan_if_compatible_with_timeinforce(Plan = #execplan{}) ->
  ok.



traverse_ets_table(Table, Key, Collector, State) ->
  KeyToUse = case Key of
    nil ->  ets:first(Table);
    _ ->    ets:next(Table, Key)
  end,
  case KeyToUse of
    '$end_of_table' -> State;
    _ ->
      [Order|_] = ets:lookup(Table, KeyToUse), % this always returns a list
      NewState = Collector(Order, State),
      if
        % if Collector returns done we stop traversing
        % and return previous good state
        NewState =:= done -> State;
        true -> traverse_ets_table(Table, KeyToUse, Collector, NewState)
      end
  end.

% ----- Events that generate Execution reports

send_accept(#order{} = Order) ->
  % nexchange_trading_book_eventmgr:notify_match(),
  ok.

send_reject(#order{} = Order, Reason) ->
  % nexchange_trading_book_eventmgr:notify_match(),
  ok.

normalize_price(Price) when is_atom(Price) -> 0;
normalize_price(Price) when is_number(Price) ->
  % remove 4 decimals by multipling by 10000
  round(Price * 10000). % round converts it back to integer


% ----- Utility functions

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
