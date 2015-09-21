-module(nexchange_sessionregistry_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
  {"The server can be started, stopped and has a registered name",
   ?setup(fun is_registered/1)}.

register_fix_session_test_() ->
  {"When a session is registed, it can be queried back",
  ?setup(fun register_and_assert/1)}.

unregister_fix_session_test_() ->
  {"When a session is unregisted, it changes the state of the registry",
  ?setup(fun unregister_and_assert/1)}.

mix_and_match_keys_for_fix_session1_test_() ->
  {"Using string or binary as key should be normalized and work",
  ?setup(fun mix_match_and_assert1/1)}.

mix_and_match_keys_for_fix_session2_test_() ->
  {"Using string or binary as key should be normalized and work",
  ?setup(fun mix_match_and_assert2/1)}.


% ---- Actual tests

is_registered({Pid,_}) ->
  [?_assert(erlang:is_process_alive(Pid)),
   ?_assertEqual(Pid, whereis(nexchange_sessionregistry))].

register_and_assert({Pid,_}) ->
  nexchange_sessionregistry:register_fixsession("Session1", Pid),
  Sessions = nexchange_sessionregistry:get_fixsessions("Session1"),
  Keys = nexchange_sessionregistry:get_registered(),
  [?_assertEqual(1, length(Sessions)),
   ?_assertEqual(1, length(Keys)),
   ?_assertMatch([Pid], Sessions),
   ?_assertMatch(["Session1"], Keys)].

unregister_and_assert({Pid,_}) ->
  nexchange_sessionregistry:register_fixsession("Session1", Pid),
  nexchange_sessionregistry:unregister_fixsession("Session1", Pid),
  Sessions = nexchange_sessionregistry:get_fixsessions("Session1"),
  Keys = nexchange_sessionregistry:get_registered(),
  [?_assertEqual(0, length(Sessions)),
   ?_assertEqual(0, length(Keys))].

mix_match_and_assert1({Pid,_}) ->
  nexchange_sessionregistry:register_fixsession(<<"Session1">>, Pid),
  Sessions = nexchange_sessionregistry:get_fixsessions("Session1"),
  Keys = nexchange_sessionregistry:get_registered(),
  [?_assertEqual(1, length(Sessions)),
   ?_assertEqual(1, length(Keys)),
   ?_assertMatch([Pid], Sessions),
   ?_assertMatch(["Session1"], Keys)].

mix_match_and_assert2({Pid,_}) ->
 nexchange_sessionregistry:register_fixsession("Session1", Pid),
 Sessions = nexchange_sessionregistry:get_fixsessions(<<"Session1">>),
 Keys = nexchange_sessionregistry:get_registered(),
 [?_assertEqual(1, length(Sessions)),
  ?_assertEqual(1, length(Keys)),
  ?_assertMatch([Pid], Sessions),
  ?_assertMatch(["Session1"], Keys)].

% ---- Setup / teardown

start() ->
  % {ok, SupPid} = nexchange_trading_sup:start_link(),
  SupPid = 0,
  {ok, Pid} = nexchange_sessionregistry:start_link(),
  {Pid, SupPid}.

stop({_Pid, SupPid}) ->
  try nexchange_sessionregistry:stop()
  catch _:_ -> ok end.
