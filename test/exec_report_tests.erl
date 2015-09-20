
-module(exec_report_tests).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").


% TODO test the conversion of order to execreport

exec_report_builder_test() ->
  % build_exec_report(_ExecType, Seq, Target, Sender)
  % ExecReport = exec_report:build_exec_report(1, 1, "target", "sender"),

  Report = #execreport{
    exec_type=new,
    from_sessionid="From", to_sessionid="To"
  },

  Bin = exec_report:report_to_fix_bin(Report, 100),

  ok.
