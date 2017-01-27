-module(option_execution_handler).

-export([handle/2, test/0, confirm_and_execute/2]).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").


test() -> 
  InitialState = #state{our_seq=1, socket=undefined},

  PR = #position_maintenance_request{
		sending_time= <<"20170125-10:10:10.354">>,
    pos_req_id = <<"28733_0">>,
    pos_trans_type = <<"1">>,
    pos_maint_action = <<"1">>,
    clearing_business_date = <<"20170124">>,
    no_party_ids = <<"1">>,
    party_id_source = <<"D">>,
    party_id = <<"98">>,
    party_role = <<"36">>,
    account = <<"4004">>,
    symbol = <<"BBDCM41E">>,
    transact_time = <<"20170124-18:13:53">>,
    no_positions = <<"1">>,
    pos_type = <<"EX">>,
    long_qty = <<"100">>,
		fields = [{msg_seq_num,17},
	            {sender_comp_id,<<"CCLRA801">>},
	            {target_comp_id,<<"OE104C">>},
	            {symbol,<<"BBDCM41E">>},
	            {cl_ord_id,<<"12345_0">>},
	            {account,<<"4004">>},
	            {transact_time,<<"20170124-18:13:53">>}
	           ]
  },

  handle(PR, InitialState).


handle(#position_maintenance_request{} = PR, State) ->

  % send a Position Maintenance Report and a exec report position

  NewState = confirm_and_execute(PR, State),

  NewState.

confirm_and_execute(PR, #state{} = State) ->
  TradeId = list_to_binary ( integer_to_list( State#state.our_seq ) ),
  %PrimaryFields = [
  %  {pos_req_id, PR#position_maintenance_request.pos_req_id },
  %  {pos_trans_type, PR#position_maintenance_request.pos_trans_type },
  %  {pos_maint_action, PR#position_maintenance_request.pos_maint_action },
  %  {clearing_business_date, PR#position_maintenance_request.clearing_business_date },
  %  {no_party_ids, PR#position_maintenance_request.no_party_ids },
  %  {party_id_source, PR#position_maintenance_request.party_id_source },
  %  {party_id, PR#position_maintenance_request.account },
  %  {party_role, PR#position_maintenance_request.party_role },
  %  {account, PR#position_maintenance_request.account },
  %  {account_type, PR#position_maintenance_request.account_type },
  %  {transact_time, PR#position_maintenance_request.transact_time },
  %  {no_positions, PR#position_maintenance_request.no_positions },
  %  {long_qty, PR#position_maintenance_request.long_qty },
  %  {trade_id, TradeId}
  %],
  %FromSessId = proplists:get_value(sender_comp_id, PR#position_maintenance_request.fields),
  %DestSessId = proplists:get_value(target_comp_id, PR#position_maintenance_request.fields),
  %Fields = [{cl_ord_id, PR#position_maintenance_request.pos_req_id},
  %          {target_comp_id, DestSessId},
  %          {sender_comp_id, FromSessId}],

  %NewState = fix_message_handler:send(position_maintenance_report, PrimaryFields, Fields, State), % AI

  ReportPosition = exec_report:build_report_for_position_maintenance_request(PR),
  exec_report_dispatcher:dispatch3(ReportPosition),
  Bin1 = exec_report:report_to_fix_bin(ReportPosition, 100),
  R1 = fix:dump(Bin1),
  ?DBG("Report Position ~p~n", [R1]),

  ReportExecution = exec_report:build_execution_report_for_position_maintenance(PR, TradeId),
  exec_report_dispatcher:dispatch2(ReportExecution),
  Bin2 = exec_report:report_to_fix_bin(ReportExecution, 100),
  R2 = fix:dump(Bin2),
  ?DBG("Executed Position ~p~n", [R2]);

confirm_and_execute(_, State) -> State.

