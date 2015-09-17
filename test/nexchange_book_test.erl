
-module(nexchange_book_test).

-compile(export_all).

-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).


start_stop_test_() ->
  {"Tests set up gen_event correctly",
   ?setup(fun is_event_manager_setup_correctly/1)}.

market_price_match_for_sell_test_() ->
  {"sell order, type: marker_price",
   ?setup(fun market_price_match_order_sell_test/1)}.

% ---- Actual tests

is_event_manager_setup_correctly(Pid) ->
  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(nexchange_trading_book_eventmgr))].


market_price_match_order_sell_test(EventMgrPid) ->
  Book = nexchange_book:create("PETR5"),

  % prepare book
  nexchange_book:insert_buy(#order{price=10, order_type=market, qtd=100, id="t1"}, Book),
  nexchange_book:insert_buy(#order{price=10, order_type=market, qtd=100, id="t1"}, Book),

  % nexchange_book:match_order(#order{  });

  nexchange_trading_book_eventmgr:notify_fill(#order{price=10}),

  All = gen_event:call(EventMgrPid, book_ev_handler, get_all),

  error_logger:info_msg("call returned  ~p ~n", All),

  [].



% ---- Setup / teardown

start() ->
  {ok, Pid} = gen_event:start_link({local, nexchange_trading_book_eventmgr}),
  gen_event:add_handler(Pid, book_ev_handler, []),
  Pid.

stop(SupPid) ->
  exit(SupPid, normal),
  Ref = monitor(process, SupPid),
  receive
    {'DOWN', Ref, process, SupPid, _Reason} -> ok
  after 1000 ->
    error(exit_timeout)
  end.


% match_sell_order_test() ->
%   Book = nexchange_book:create(),
%
%   nexchange_book:insert_buy(#order{price = 10, time = 0, qtd = 100, id = "t1"}, Book),
%   nexchange_book:insert_buy(#order{price = 10, time = 0, qtd = 100, id = "t2"}, Book),
%   nexchange_book:insert_buy(#order{price =  9, time = 0, qtd = 100, id = "t3"}, Book),
%   nexchange_book:insert_buy(#order{price =  9, time = 0, qtd = 100, id = "t4"}, Book),
%   nexchange_book:insert_buy(#order{price = 11, time = 0, qtd = 100, id = "t5"}, Book),
%
%   Selection = nexchange_book:match_sell_order(#order{price=9, time = 0, qtd=150, id="s1"}, Book),
%
%   % error_logger:info_msg("Result is ~p ~n", Selection).
%   ok.
%
%
%
% sells_book_has_proper_order_test() ->
%   Book = nexchange_book:create(),
%
%   nexchange_book:insert_sell(#order{price = 10, time = 0, qtd = 100, id = "t1"}, Book),
%   nexchange_book:insert_sell(#order{price = 10, time = 0, qtd = 100, id = "t2"}, Book),
%   nexchange_book:insert_sell(#order{price =  9, time = 0, qtd = 100, id = "t3"}, Book),
%
%   % Collector = fun (Item,S) -> error_logger:info_msg("Item is ~p ~n", hd(Item)#order.id ), S end,
%   Collector = fun (Item, S) -> S ++ [Item#order.id] end,
%
%   Result = nexchange_book:list_sells(Book, Collector, []),
%
%   ?assertEqual(["t3","t1","t2"], Result).
%   %error_logger:info_msg("Result is ~p ~n", Result).
%
%
%
% buy_book_has_proper_order_test() ->
%   Book = nexchange_book:create(),
%
%   nexchange_book:insert_buy(#order{price = 10, time = 0, qtd = 100, id = "t1"}, Book),
%   nexchange_book:insert_buy(#order{price = 10, time = 0, qtd = 100, id = "t2"}, Book),
%   nexchange_book:insert_buy(#order{price =  9, time = 0, qtd = 100, id = "t3"}, Book),
%   nexchange_book:insert_buy(#order{price = 11, time = 0, qtd = 100, id = "t4"}, Book),
%
%   Collector = fun (Item, S) -> S ++ [Item#order.id] end,
%
%   Result = nexchange_book:list_buys(Book, Collector, []),
%
%   ?assertEqual(["t4","t1","t2", "t3"], Result).
%   %error_logger:info_msg("Result is ~p ~n", Result).


%
% normalize_price_test() ->
%   ?assertEqual(nexchange_book:normalize_price(10), 100000),
%   ?assertEqual(nexchange_book:normalize_price(10.1), 101000),
%   ?assertEqual(nexchange_book:normalize_price(10.1234), 101234).
%
% sell_order_key_same_price_older_should_come_first_test() ->
%   {Key1,_} = nexchange_book:sell_order_key(12),
%   {Key2,_}  = nexchange_book:sell_order_key(12),
%   ?assert(Key1 < Key2).
%
% sell_order_key_cheaper_should_take_precedence_test() ->
%   {Key1,_}  = nexchange_book:sell_order_key(12),
%   {Key2,_}  = nexchange_book:sell_order_key(11),
%   ?assert(Key2 < Key1).
%
% buy_order_key_same_price_older_should_come_first_test() ->
%   {Key1,_}  = nexchange_book:buy_order_key(12),
%   {Key2,_}  = nexchange_book:buy_order_key(12),
%   % error_logger:info_msg("Key is ~p and ~p ~n", [Key1, Key2]),
%   ?assert(Key1 < Key2).
%
% buy_order_key_higher_price_should_take_precedence_test() ->
%   {Key1,_}  = nexchange_book:buy_order_key(12),
%   {Key2,_}  = nexchange_book:buy_order_key(13),
%   % error_logger:info_msg("Key 1 for 12 is ~p and 2 for 13 is ~p ~n", [Key1, Key2]),
%   ?assert(Key2 < Key1).
