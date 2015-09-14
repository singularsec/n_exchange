-module(nexchange_fixsession_eventmgr).

-export([setup_handlers/0, notify_session_authenticated/2, notify_session_terminated/2]).

% API

setup_handlers() ->
  ok = gen_event:add_handler(nexchange_fixsession_eventmgr, nexchange_fixsession_registry_handler, []),
  ok.

notify_session_authenticated(SessionId, Pid) when is_pid(Pid) ->
  gen_event:notify(nexchange_fixsession_eventmgr, {session_authenticated, SessionId, Pid}).

notify_session_terminated(SessionId, Pid) when is_pid(Pid) ->
  gen_event:notify(nexchange_fixsession_eventmgr, {session_terminated, SessionId, Pid}).
