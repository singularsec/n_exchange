-module(nexchange_fixsession_eventmgr).

-export([notify_session_authenticated/2, notify_session_terminated/2]).

% API

% add_handler() ->
%   ok.

notify_session_authenticated(SessionId, Pid) when is_pid(Pid) ->
  gen_event:notify(nexchange_fixsession_eventmgr, {session_authenticated, SessionId, Pid}).

notify_session_terminated(SessionId, Pid) when is_pid(Pid) ->
  gen_event:notify(nexchange_fixsession_eventmgr, {session_terminated, SessionId, Pid}).
