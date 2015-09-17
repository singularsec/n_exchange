
-module(nexchange_book).

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(export_all).
% -export([create/0, match_sell_order/2, match_buy_order/2]).
-endif.

-include("../include/secexchange.hrl").






create(Symbol) ->
  BuyTName  = list_to_atom( atom_to_list(buy)  ++ "_" ++ Symbol ),
  SellTName = list_to_atom( atom_to_list(sell) ++ "_" ++ Symbol ),

  BuysT  = ets:new(BuyTName,  [ordered_set, {keypos, #order.oid}]),
  SellsT = ets:new(SellTName, [ordered_set, {keypos, #order.oid}]),
  Book   = #book{buys=BuysT, sells=SellsT},
  Book.

%% TODO: market price with protection
%% TODO: limit orders
%% TODO: stop order with protection
%% TODO: stop order limit
%% TODO: market with leftover as limit


match_order(#order{} = _Order, _Book) ->

  % post match

  nexchange_trading_book_eventmgr:notify_match(),

  ok.

cancel_order(#order{} = _Order, _Book) ->
  ok.

change_order(#order{} = _Order, _Book) ->
  ok.


match_sell_order(#order{} = Order, Book) ->
  % if is valid
  % notify_accepted()

  SellOrder = insert_sell(Order, Book),
  Collector = fun (Item, {Selected, QtdToFill, DesiredPrice} ) ->
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
  Result = list_buys(Book, Collector, {[], SellOrder#order.qtd, SellOrder#order.price}),
  Result.


match_buy_order(#order{} = _Order, _Book) ->
  % BuyOrder = insert_buy(Order, Book),
  ok.

insert_sell(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{sells = Sells}) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = sell_order_key(NormalizedPrice),
  NewOrder = Order#order{oid = Key, time = Time, price=NormalizedPrice},
  ets:insert(Sells, NewOrder),
  NewOrder.

insert_buy(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{buys = Buys}) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = buy_order_key(NormalizedPrice),
  NewOrder = Order#order{oid = Key, time = Time, price=NormalizedPrice},
  ets:insert(Buys, NewOrder),
  NewOrder.

list_sells(#book{sells = Sells}, Collector, State) ->
  traverse_ets_table(Sells, nil, Collector, State).

list_buys(#book{buys = Buys}, Collector, State) ->
  traverse_ets_table(Buys, nil, Collector, State).

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

get_now() -> erlang:system_time(micro_seconds). % - 1441736774862944.

sell_order_key(Price) ->
  Now = get_now(),
  Res = (Price * 100000000000000000) + Now,
  {Res, Now}.

buy_order_key(Price) ->
  Now = get_now(),
  Res = (Price * -100000000000000000) + Now,
  {Res, Now}.

normalize_price(Price) ->
  % remove 4 decimals by multipling by 10000
  round(Price * 10000). % round converts it back to integer



%
