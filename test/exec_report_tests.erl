
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
  {ok, #execution_report{sending_time=_S} = HB,_Rest,_C} = fix:decode( iolist_to_binary(Bin) ),

  error_logger:info_msg("Result ~n ~p ~n ~p ~n", [HB, fix:dump(Bin)]),

  ok.
