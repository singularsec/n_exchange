-module(n_orderbook_test).

-compile(export_all).

-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).



start_stop_test_() ->
  {"Tests set up gen_event correctly",
   ?setup(fun is_event_manager_setup_correctly/1)}.

market_price_match_for_sell_test_() ->
  {"sell order, type: limit",
   ?setup(fun limit_price_match_order_sell_test/1)}.


% ---- Actual tests

is_event_manager_setup_correctly(Pid) ->
  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(nexchange_trading_book_eventmgr))].


limit_price_match_order_sell_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),

  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=100, side=buy, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=100, side=buy, cl_ord_id="t2"}, Book),

  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=200, side=sell, cl_ord_id="t3"}, Book),

  % nexchange_trading_book_eventmgr:notify_fill(#order{price=10}),

  All = gen_event:call(EventMgrPid, book_ev_handler, get_all),

  % Item#order.price, {qtd,Item#order.qtd}, {filled,Item#order.qtd_filled}, {left, Item#order.qtd_left}, {last,Item#order.qtd_last}

  % All2 = lists:map(fun ({Msg,Item}) -> {Msg,
  %   Item#order.cl_ord_id,
  %   Item#order.price / 10000,
  %   {qtd,Item#order.qtd},
  %   {filled,Item#order.qtd_filled},
  %   {left, Item#order.qtd_left},
  %   {last,Item#order.qtd_last}} end, All),

  error_logger:info_msg("R ~p ~n", [All]),

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
