-module(exec_report_dispatcher).

-export([dispatch/1, dispatch/2, dispatch2/1, dispatch3/1]).

-include("log.hrl").
% -include("../include/fix_session.hrl").
% -include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").


dispatch3(Report = #position_maintenance_report{to_sessionid=SessionId}) ->
  dispatch(Report, SessionId).

dispatch2(Report = #execreport{from_sessionid=SessionId}) ->
  dispatch(Report, SessionId).

dispatch(Report = #cancelreject{to_sessionid=SessionId}) ->
  dispatch(Report, SessionId);

dispatch(Report = #execreport{to_sessionid=SessionId}) ->
  dispatch(Report, SessionId).

dispatch(Report, #order{from_sessionid=SessionId}) ->
  dispatch(Report, SessionId);

dispatch(Report, SessionId) when is_binary(SessionId) ->
  dispatch(Report, binary_to_list(SessionId));

dispatch(Report, SessionId) when is_list(SessionId) ->
  Pids = nexchange_sessionregistry:get_fixsessions(SessionId),
  multi_cast(Pids, {send, Report}).



multi_cast([], _Request) -> ok;

multi_cast([Pid|_], Request) ->
  gen_server:cast(Pid, Request).
