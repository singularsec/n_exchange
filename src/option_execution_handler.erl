-module(option_execution_handler).

-export([handle/2, confirm_and_execute/2]).

-include("log.hrl").
-include("../include/fix_session.hrl").
%-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

handle(#position_maintenance_request{} = PR, State) ->
  % send a Position Maintenance Report and a exec report position
  NewState = confirm_and_execute(PR, State),
  NewState.

confirm_and_execute(PR, #state{} = State) ->
  ?DBG("Received!", [PR]),

  TradeId = list_to_binary ( integer_to_list( State#state.our_seq ) ),
  Parties = fix_utils:extract_parties(PR#position_maintenance_request.fields),
  PrimaryFields =
  [
    {pos_req_id, PR#position_maintenance_request.pos_req_id },
    {pos_maint_status, 3}, %COMPLETED
    %pos_maint_result, reason for rejection
    {pos_trans_type, PR#position_maintenance_request.pos_trans_type },
    {pos_maint_action, PR#position_maintenance_request.pos_maint_action },
    {clearing_business_date, PR#position_maintenance_request.clearing_business_date },
    %{no_party_ids, PR#position_maintenance_request.no_party_ids },
    {no_party_ids, 0},
    %{parties, Parties},
    {account, PR#position_maintenance_request.account},
    {symbol, PR#position_maintenance_request.symbol},
    %{account_type, PR#position_maintenance_request.account_type },  ???????? TENHO ISTO????
    {transact_time, PR#position_maintenance_request.transact_time },
    {{no_positions, PR#position_maintenance_request.no_positions },
      {pos_type, PR#position_maintenance_request.pos_type },
      {long_qty, PR#position_maintenance_request.long_qty }} ,
    %{threshold_amount, PR#position_maintenance_request.threshold_amount },  ???????? TENHO ISTO????
    {pos_maint_rpt_id, <<"abobrinha">>}
    %{trade_id, TradeId}
  ],

  FromSessId = proplists:get_value(sender_comp_id, PR#position_maintenance_request.fields),
  DestSessId = proplists:get_value(target_comp_id, PR#position_maintenance_request.fields),

  Fields = [{cl_ord_id, PR#position_maintenance_request.pos_req_id},
          {target_comp_id, DestSessId},
          {sender_comp_id, FromSessId}
  ],

  ?DBG("Sending Report !", [PrimaryFields, Fields]),

  NewState = fix_message_handler:send(position_maintenance_report, PrimaryFields, Fields, State),
  %?DBG("position_maintenance_request Received! PrimaryFields / Fields ", [PrimaryFields, Fields]),

  ReportExecution = exec_report:build_execution_report_for_position_maintenance(PR, TradeId),
  ?DBG("position_maintenance_request Executed!", [ReportExecution]),

  exec_report_dispatcher:dispatch2(ReportExecution),
  Bin2 = exec_report:report_to_fix_bin(ReportExecution, 100),
  R2 = fix:dump(Bin2),

  %?DBG("position_maintenance_request Executed! ", [R2]),

  NewState.
