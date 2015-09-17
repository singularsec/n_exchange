-module(nexchange_trading_book_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start_sup/0, fun stop_sup/1, F}).

start_stop_test_() ->
  {"The sup can be started, stopped and has a registered name",
   ?setup(fun is_registered/1)}.

create_book_test_() ->
 {"Book can be created and replies to message",
  ?setup(fun creates_book/1)}.

wont_duplicate_book_test_() ->
 {"Diff symbols get diff book pids",
  ?setup(fun wont_duplicate/1)}.

% ---- Actual tests

is_registered(Pid) ->
  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(nexchange_trading_sup))].

creates_book(_) ->
  BookPid = new_book("PETR"),
  pong = nexchange_trading_book:ping(BookPid),
  [?_assert( is_pid(BookPid) )].

wont_duplicate(_) ->
  BookPid1 = new_book("PETR3"),
  BookPid2 = new_book("PETR4"),
  [?_assertNotEqual(BookPid1, BookPid2)].

% ---- Setup / teardown

new_book(Symbol) ->
  nexchange_trading_sup:create_book(Symbol).

start_sup() ->
   {ok, Pid} = nexchange_trading_sup:start_link(),
   Pid.

stop_sup(Pid) ->
  exit(Pid, normal),

  Ref = monitor(process, Pid),
  receive {'DOWN', Ref, process, Pid, _Reason} -> ok
  after 1000 -> error(exit_timeout)
  end.
