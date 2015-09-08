
-module(nexchange_book).

-compile(export_all).

-include("../include/secexchange.hrl").

create() ->
  BuysT  = ets:new(buys,  [ordered_set, {keypos, #order.oid}]),
  SellsT = ets:new(sells, [ordered_set, {keypos, #order.oid}]),
  Book = #book{buys=BuysT, sells=SellsT},
  Book.

% -record(order, {price, time, qtd, id}).
insert_sell(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{sells = Sells} = Book) ->
  {Key, Time} = order_key(Price),
  NewOrder = Order#order{oid = Key, time = Time},
  ets:insert(Sells, NewOrder),
  Book.
  % NewSells = Sells ++ [Order#order{oid = Key, time = Time}],
  % NewBook = #book{sells=NewSells, buys=Book#book.buys},
  % NewBook.

insert_buy(#order{price = Price, time = _T, qtd = _Qtd, id = _Id} = Order,
            #book{buys = Buys} = Book) ->
  {Key, Time} = order_key_inverted(Price),
  NewOrder = Order#order{oid = Key, time = Time},
  ets:insert(Buys, NewOrder),
  Book.

dump_info(#book{sells = Sells, buys = Buys}) ->
  lists:foldr (fun (E, Acc) -> Acc + " 1 " end, "", Sells).

% cancel, replace, change

order_key(Price) ->
  % TODO: need to normalize number removing decimals
  Now = erlang:system_time(micro_seconds),
  Res = (Price * 100000000000000000) + Now,
  {Res, Now}.

order_key_inverted(Price) ->
  Now = erlang:system_time(micro_seconds) * -1,
  Res = (Price * 100000000000000000) + Now,
  {Res, Now}.
