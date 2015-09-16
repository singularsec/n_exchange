
-module(exec_report_tests).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

exec_report_builder_test() ->
  % build_exec_report(_ExecType, Seq, Target, Sender)
  % ExecReport = exec_report:build_exec_report(1, 1, "target", "sender"),

  ok.
