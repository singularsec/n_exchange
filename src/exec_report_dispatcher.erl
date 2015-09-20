-module(exec_report_dispatcher).

-export([dispatch/2]).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").


% Report is execreport from secexchange
% Order is order from secexchange
dispatch(Report, Order=#order{from_sessionid=SessionId}) ->

  Pids = nexchange_sessionregistry:get_fixsessions(SessionId),

  multi_cast(Pids, {send, Report}).



multi_cast([], _Request) -> ok;

multi_cast([Pid|Rest], Request) ->
  gen_server:cast(Pid, Request).

