-module(nexchange_bookregistry_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
  {"The server can be started, stopped and has a registered name",
   ?setup(fun is_registered/1)}.

empty_list_test_() ->
 {"Once started, no book is registered",
  ?setup(fun ({Pid,_}) ->
    Keys = nexchange_bookregistry:get_registered(),
    [?_assertEqual(0, length(Keys))]
  end)}.

registering_symbol_many_times_test_() ->
  {"A symbol is registered only once",
  ?setup(fun register_many/1)}.

unregistering_symbol_removes_it_test_() ->
  {"Removing a symbol remove its book",
  ?setup(fun register_and_unregister/1)}.

is_registered({Pid,_}) ->
   [?_assert(erlang:is_process_alive(Pid)),
    ?_assertEqual(Pid, whereis(nexchange_bookregistry))].

register_many({_Pid,_}) ->
  Pid1 = nexchange_bookregistry:get_book("PETR5"),
  Pid2 = nexchange_bookregistry:get_book("PETR5"),
  Keys = nexchange_bookregistry:get_registered(),
  [?_assertEqual(Pid1, Pid2), ?_assertEqual(1, length(Keys))].

register_and_unregister({_Pid,_SupId}) ->
  Pid1 = nexchange_bookregistry:get_book("PETR5"),
  nexchange_bookregistry:book_removed("PETR5", Pid1),
  Keys = nexchange_bookregistry:get_registered(),
  [?_assertEqual(0, length(Keys))].

start() ->
   {ok, SupPid} = nexchange_trading_sup:start_link(),
   {ok, Pid} = nexchange_bookregistry:start_link(),
   %  error_logger:info_msg("Started with ~p ~n", [self(), Pid, SupPid]),
   {Pid, SupPid}.

stop({_Pid, SupPid}) ->
  try nexchange_bookregistry:stop()
  catch _:_ -> ok end,
  exit(SupPid, normal),
  Ref = monitor(process, SupPid),
  receive
    {'DOWN', Ref, process, SupPid, _Reason} -> ok
  after 1000 ->
    error(exit_timeout)
  end.
