-module(exec_report_dispatcher).

-export([dispatch/1, dispatch/2]).

-include("log.hrl").
% -include("../include/fix_session.hrl").
% -include("../include/admin44.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").

dispatch(Report = #execreport{to_sessionid=SessionId}) ->
  dispatch(Report, SessionId).

dispatch(Report, Order=#order{from_sessionid=SessionId}) ->
  dispatch(Report, SessionId);

dispatch(Report, SessionId) when is_binary(SessionId) ->
  dispatch(Report, binary_to_list(SessionId));

dispatch(Report, SessionId) when is_list(SessionId) ->
  Pids = nexchange_sessionregistry:get_fixsessions(SessionId),
  multi_cast(Pids, {send, Report}).





multi_cast([], _Request) -> ok;

multi_cast([Pid|Rest], Request) ->
  gen_server:cast(Pid, Request).
