-module(n_orderbook_test).

-compile(export_all).

-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
  {"Tests set up gen_event correctly",
   ?setup(fun is_event_manager_setup_correctly/1)}.

limit_matching_1_test_() ->
  {"sells at 10, buys at 11, matches at 10",
   ?setup(fun limit_sell_first_test/1)}.

limit_matching_2_test_() ->
  {"buys at 11, sells 10, matches at 11",
   ?setup(fun limit_buy_first_test/1)}.

limit_matching_3_test_() ->
  {"buys at 11, two sells at 9 and 10",
   ?setup(fun limit_buy_first_2_test/1)}.

limit_matching_4_test_() ->
  {"buys at 11, sells at 11 and 12",
   ?setup(fun limit_sell_first_2_test/1)}.

limit_matching_time_in_force_is_immediateorcancel_success_test_() ->
  {"simple buy and sell with timeinforce = immediateorcancel succeeds",
   ?setup(fun limit_matching_time_in_force_is_immediateorcancel_test/1)}.

% limit_matching_time_in_force_is_immediateorcancel_cancel_leftover_test_() ->
%   {"simple buy and sell with timeinforce = immediateorcancel succeeds cancels qtd unmatched",
%    ?setup(fun limit_matching_time_in_force_is_immediateorcancel_leftover_test/1)}.

% limit_matching_time_in_force_is_immediateorcancel_cancel_leftover_test_() ->
%   {"buys at 11, sells at 11 and 12",
%    ?setup(fun limit_matching_time_in_force_is_immediateorcancel_leftover_test/1)}.
%
% limit_matching_time_in_force_is_fillorkill_success_test_() ->
%   {"buys at 11, sells at 11 and 12",
%    ?setup(fun limit_matching_time_in_force_is_fillorkill_test/1)}.
%
% limit_matching_time_in_force_is_fillorkill_cancel_test_() ->
%  {"buys at 11, sells at 11 and 12",
%   ?setup(fun limit_matching_time_in_force_is_fillorkill_test/1)}.


% ---- Actual tests

is_event_manager_setup_correctly(Pid) ->
  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(nexchange_trading_book_eventmgr))].


limit_matching_time_in_force_is_immediateorcancel_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=100, side=sell, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=buy,  cl_ord_id="t2", timeinforce=immediateorcancel}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [ {accept,    "t1", 10.0, sell, new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {accept,    "t2", 11.0, buy,  new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {full_fill, "t1", 10.0, sell, filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{10.0,100}]}},
    {full_fill, "t2", 11.0, buy,  filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{10.0,100}]}}
  ], All).


limit_matching_time_in_force_is_immediateorcancel_leftover_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=50,  side=sell, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=buy,  cl_ord_id="t2", timeinforce=immediateorcancel}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [
    {accept,       "t1", 10.0, sell, new,       {qtd, 50}, {filled, 0},   {left,  50}, {last, 0},   {matches, []}},
    {accept,       "t2", 11.0, buy,  new,       {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {full_fill,    "t1", 10.0, sell, filled,    {qtd,50},  {filled, 50},  {left, 0},   {last, 50},  {matches, [{10.0,50}]}},
    {partial_fill, "t2", 11.0, buy,  partial,   {qtd,100}, {filled, 50},  {left, 50},  {last, 50},  {matches, [{10.0,50}]}},
    {cancel,       "t2", 11.0, buy,  canceled,  {qtd,100}, {filled, 50},  {left, 50},  {last, 50},  {matches, [{10.0,50}]}, "unexecuted quantity cancelled"}
  ], All).


limit_sell_first_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=100, side=sell, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=buy, cl_ord_id="t2"}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [
    {accept,    "t1", 10.0, sell, new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {accept,    "t2", 11.0, buy,  new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {full_fill, "t1", 10.0, sell, filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{10.0,100}]}},
    {full_fill, "t2", 11.0, buy,  filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{10.0,100}]}}
  ], All).


limit_buy_first_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=buy, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=100, side=sell, cl_ord_id="t2"}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [
    {accept,    "t1", 11.0, buy,  new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {accept,    "t2", 10.0, sell, new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {full_fill, "t1", 11.0, buy,  filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{11.0,100}]}},
    {full_fill, "t2", 10.0, sell, filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 100}, {matches, [{11.0,100}]}}
  ], All).


limit_buy_first_2_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=buy,  cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=10, order_type=limit, qtd=50,  side=sell, cl_ord_id="t2"}, Book),
  n_orderbook:add_new_order_single(#order{price=9,  order_type=limit, qtd=50,  side=sell, cl_ord_id="t3"}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [
    {accept,       "t1", 11.0, buy,  new,    {qtd,100}, {filled, 0},   {left, 100}, {last, 0},   {matches, []}},
    {accept,       "t2", 10.0, sell, new,    {qtd, 50}, {filled, 0},   {left, 50},  {last, 0},   {matches, []}},
    {partial_fill, "t1", 11.0, buy, partial, {qtd,100}, {filled, 50},  {left, 50},  {last, 50},  {matches, [{11.0,50}]}},
    {full_fill,    "t2", 10.0, sell, filled, {qtd,50},  {filled, 50},  {left, 0},   {last, 50},  {matches, [{11.0,50}]}},
    {accept,       "t3",  9.0, sell, new,    {qtd, 50}, {filled, 0},   {left, 50},  {last, 0},   {matches, []}},
    {full_fill,    "t1", 11.0, buy,  filled, {qtd,100}, {filled, 100}, {left, 0},   {last, 50},  {matches, [{11.0,50},{11.0,50}]}},
    {full_fill,    "t3",  9.0, sell, filled, {qtd,50},  {filled, 50},  {left, 0},   {last, 50},  {matches, [{11.0,50}]}}
  ], All).


limit_sell_first_2_test(EventMgrPid) ->
  Book = n_orderbook:create("PETR5"),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=100, side=sell, cl_ord_id="t1"}, Book),
  n_orderbook:add_new_order_single(#order{price=11, order_type=limit, qtd=50,  side=buy,  cl_ord_id="t2"}, Book),
  n_orderbook:add_new_order_single(#order{price=12, order_type=limit, qtd=50,  side=buy,  cl_ord_id="t3"}, Book),
  All = collect_events(EventMgrPid),
  % error_logger:info_msg("R ~p ~n", [All]),
  assertMatch(
  [
    {accept,       "t1", 11.0, sell, new,    {qtd,100},{filled,0},  {left,100},{last,0},  {matches,[]}},
    {accept,       "t2", 11.0, buy,  new,    {qtd,50}, {filled,0},  {left,50}, {last,0},  {matches,[]}},
    {partial_fill, "t1", 11.0, sell, partial,{qtd,100},{filled,50}, {left,50}, {last,50}, {matches,[{11.0,50}]}},
    {full_fill,    "t2", 11.0, buy,  filled, {qtd,50}, {filled,50}, {left,0},  {last,50}, {matches,[{11.0,50}]}},
    {accept,       "t3", 12.0, buy,  new,    {qtd,50}, {filled,0},  {left,50}, {last,0},  {matches,[]}},
    {full_fill,    "t1", 11.0, sell, filled, {qtd,100},{filled,100},{left,0},  {last,50}, {matches,[{11.0,50},{11.0,50}]}},
    {full_fill,    "t3", 12.0, buy,  filled, {qtd,50}, {filled,50}, {left,0},  {last,50}, {matches,[{11.0,50}]}}
  ], All).


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

% ---- Utility

assertMatch([], []) -> [];

assertMatch([Match|Rest], [Received|More]) ->
  Asm = ?_assertMatch(Match, Received),
  [ Asm ] ++ assertMatch(Rest, More).


translate({Msg, {#order{cl_ord_id=ClOrdId} = Item, Reason}}) ->
  {Msg,
   ClOrdId,
   Item#order.price / 10000,
   Item#order.side,
   Item#order.order_status,
   {qtd, Item#order.qtd},
   {filled, Item#order.qtd_filled},
   {left, Item#order.qtd_left},
   {last, Item#order.qtd_last},
   {matches, strip_matches(Item#order.matches)}, Reason};

translate({Msg, #order{cl_ord_id=ClOrdId} = Item}) ->
  {Msg,
   ClOrdId,
   Item#order.price / 10000,
   Item#order.side,
   Item#order.order_status,
   {qtd, Item#order.qtd},
   {filled, Item#order.qtd_filled},
   {left, Item#order.qtd_left},
   {last, Item#order.qtd_last},
   {matches, strip_matches(Item#order.matches)}}.

collect_events(EventMgrPid) ->
  All = gen_event:call(EventMgrPid, book_ev_handler, get_all),
  lists:map(fun translate/1, All).

strip_matches(Matches) ->
  lists:map(fun ({P,Q,_SecOrder}) -> {P/10000, Q} end, Matches).




%
