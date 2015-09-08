
-module(nexchange_book).

-ifdef(EUNIT).
-compile(export_all).
-else.
-compile(export_all).
% -export([]).
-endif.

-include("../include/secexchange.hrl").

create() ->
  BuysT  = ets:new(buys,  [ordered_set, {keypos, #order.oid}]),
  SellsT = ets:new(sells, [ordered_set, {keypos, #order.oid}]),
  Book = #book{buys=BuysT, sells=SellsT},
  Book.

% -record(order, {price, time, qtd, id}).
insert_sell(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{sells = Sells} = Book) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = sell_order_key(NormalizedPrice),
  NewOrder = Order#order{oid = Key, time = Time},
  ets:insert(Sells, NewOrder),
  Book.
  % NewSells = Sells ++ [Order#order{oid = Key, time = Time}],
  % NewBook = #book{sells=NewSells, buys=Book#book.buys},
  % NewBook.

insert_buy(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{buys = Buys} = Book) ->
  NormalizedPrice = normalize_price(Price),
  {Key, Time} = buy_order_key(NormalizedPrice),
  NewOrder = Order#order{oid = Key, time = Time},
  ets:insert(Buys, NewOrder),
  Book.

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
    '$end_of_table' ->
      State;
    _ ->
      Order = ets:lookup(Table, KeyToUse),
      NewState = Collector(Order, State),
      traverse_ets_table(Table, KeyToUse, Collector, NewState)
  end.

% cancel, replace, change
get_now() ->
  erlang:system_time(micro_seconds). % - 1441736774862944.

sell_order_key(Price) ->
  % TODO: need to normalize number removing decimals
  Now = get_now(),
  Res = (Price * 100000000000000000) + Now,
  {Res, Now}.

buy_order_key(Price) ->
  % TODO: need to normalize number removing decimals
  Now = get_now(),
  Res = ((Price * -100000000000000000) + Now * 1),
  {Res, Now}.

normalize_price(Price) ->
  % remove 4 decimals by multipling by 10000
  round(Price * 10000). % round converts it back to integer
