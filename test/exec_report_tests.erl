
-module(exec_report_tests).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").


% TODO test the conversion of order to execreport

% exec_report_builder_with_exec_type_only_test() ->
%   Report = #execreport{
%     exec_type=new,from_sessionid="From", to_sessionid="To"
%   },
%   {ER, _} = back_and_forth(Report),
%   ?_assertEqual(new, ER#execution_report.exec_type).
%
% exec_report_builder_with_party_test() ->
%   Report = #execreport{
%     from_sessionid="From", to_sessionid="To",
%     parties=[#execparty{id=1,source="s",role=2}]
%   },
%   {ER, _} = back_and_forth(Report),
%   Fields = ER#execution_report.fields,
%   ?_assertEqual(1, proplists:get_value(no_party_ids, Fields)),
%   ?_assertEqual("1", proplists:get_value(party_id, Fields)),
%   ?_assertEqual("s", proplists:get_value(party_id_source, Fields)),
%   ?_assertEqual(2, proplists:get_value(party_role, Fields)).
%
% exec_report_builder_with_contra_brokers_test() ->
%   Report = #execreport{
%     from_sessionid="From", to_sessionid="To",
%     contrabrokers=[1,2,3]
%   },
%   {ER, _} = back_and_forth(Report),
%   Fields = ER#execution_report.fields,
%   ?_assertEqual(3, proplists:get_value(no_contra_brokers, Fields)).
%
% exec_report_builder_fields_test() ->
%   Report = #execreport{
%       order_id="orderid",
%       secondary_order_id="secorderid",
%       exec_id=0,
%       exec_type=fill,
%       order_status=filled,
%       % order_type=,
%       cl_ord_id="clid",
%       orig_cl_ord_id="orgclid",
%       account=1234,
%       symbol="PETR4",
%       side=buy,
%       time_in_force=day,
%       transact_time="time", trade_date="date",
%       qtd=#execreportqtd{order_qtd=100, last=1, leaves=2, cum=3},
%       price=#execreportprice{avg=12, last=13, price=14},
%       % text,   % for error messages
%       from_sessionid="From", to_sessionid="To"
%   },
%   {ER, RBin} = back_and_forth(Report),
%   error_logger:info_msg("Result ~n ~p ~n ~p ~n", [ER, fix:dump(RBin)]),
%   Fields = ER#execution_report.fields,
%   ?_assertEqual(0, proplists:get_value(no_contra_brokers, Fields)),
%   ?_assertEqual(0, proplists:get_value(no_party_ids, Fields))
%   .

exec_report_builder_fields2_test() ->
  Report = #execreport{
      order_id="orderid",
      from_sessionid="From", to_sessionid="To"
  },
  {ER, RBin} = back_and_forth(Report),
  error_logger:info_msg("Result ~n ~p ~n ~p ~n", [ER, fix:dump(RBin)]),
  Fields = ER#execution_report.fields,
  ?_assertEqual(0, proplists:get_value(no_contra_brokers, Fields)),
  ?_assertEqual(0, proplists:get_value(no_party_ids, Fields))
  .


back_and_forth(Report) ->
  Bin = exec_report:report_to_fix_bin(Report, 100),
  {ok, #execution_report{sending_time=_S} = ER,_Rest,_C} = fix:decode( iolist_to_binary(Bin) ),
  error_logger:info_msg("Result ~n ~p ~n ~p ~n", [ER, fix:dump(Bin)]),
  {ER, fix:dump(Bin)}.





%
