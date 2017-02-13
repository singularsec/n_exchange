-module(option_execution_handler).

-export([handle/4, confirm_and_execute/2]).

-include("log.hrl").
-include("../include/fix_session.hrl").
%-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

handle(#position_maintenance_request{} = PR, Messages, Rest, #state{} = State) ->
  % send a Position Maintenance Report and a exec report position

  NewState = confirm_and_execute(PR, State),

  fix_message_handler:handle_messages(Messages, Rest, NewState).


confirm_and_execute(#position_maintenance_request{} = PR, #state{} = State) ->
  ?DBG("Received! ~n~p~n", [PR]),

  %TradeId = list_to_binary ( integer_to_list( State#state.our_seq ) ),
  %Parties = fix_utils:extract_parties(PR#position_maintenance_request.fields),

  RequestRejected =  (PR#position_maintenance_request.symbol == <<"RAPTA5E">>),

  if
    RequestRejected ->
      PrimaryFields =
        [
          {pos_req_id, PR#position_maintenance_request.pos_req_id },
          {pos_maint_status, 2}, %REJECTED
          {pos_maint_result, 1}, %REJECTED
          {pos_trans_type, PR#position_maintenance_request.pos_trans_type },
          {pos_maint_action, PR#position_maintenance_request.pos_maint_action },
          {clearing_business_date, PR#position_maintenance_request.clearing_business_date },
          %{no_party_ids, PR#position_maintenance_request.no_party_ids },
          {no_party_ids, 0},
          %{parties, Parties},
          {account, PR#position_maintenance_request.account},
          {symbol, PR#position_maintenance_request.symbol},
          %{account_type, PR#position_maintenance_request.account_type },
          {transact_time, PR#position_maintenance_request.transact_time },
          {{no_positions, PR#position_maintenance_request.no_positions },
            {pos_type, PR#position_maintenance_request.pos_type },
            {long_qty, PR#position_maintenance_request.long_qty }} ,
          %{threshold_amount, PR#position_maintenance_request.threshold_amount },
          {pos_maint_rpt_id, <<"abobrinha">>}
          %{trade_id, TradeId}
        ];
    true ->
      PrimaryFields =
      [
        {pos_req_id, PR#position_maintenance_request.pos_req_id },
        {pos_maint_status, 3}, %COMPLETED
        {pos_trans_type, PR#position_maintenance_request.pos_trans_type },
        {pos_maint_action, PR#position_maintenance_request.pos_maint_action },
        {clearing_business_date, PR#position_maintenance_request.clearing_business_date },
        %{no_party_ids, PR#position_maintenance_request.no_party_ids },
        {no_party_ids, 0},
        %{parties, Parties},
        {account, PR#position_maintenance_request.account},
        {symbol, PR#position_maintenance_request.symbol},
        %{account_type, PR#position_maintenance_request.account_type },
        {transact_time, PR#position_maintenance_request.transact_time },
        {{no_positions, PR#position_maintenance_request.no_positions },
        {pos_type, PR#position_maintenance_request.pos_type },
        {long_qty, PR#position_maintenance_request.long_qty }} ,
        %{threshold_amount, PR#position_maintenance_request.threshold_amount },
        {pos_maint_rpt_id, <<"abobrinha">>}
        %{trade_id, TradeId}
      ]
  end,

  FromSessId = proplists:get_value(sender_comp_id, PR#position_maintenance_request.fields),
  DestSessId = proplists:get_value(target_comp_id, PR#position_maintenance_request.fields),

  Fields = [{cl_ord_id, PR#position_maintenance_request.pos_req_id},
    {target_comp_id, DestSessId},
    {sender_comp_id, FromSessId}
  ],

  ?DBG("Sending Report ! ~n~p~n~p~n", [PrimaryFields, Fields]),

  NewState = fix_message_handler:send(position_maintenance_report, PrimaryFields, Fields, State),
  %?DBG("position_maintenance_request Received! PrimaryFields / Fields ", [PrimaryFields, Fields]),

  if
    RequestRejected ->
      ?DBG("position_maintenance_request REJECTED *** ", []);

    false -> %nao envia os ExecutionReport
      %Gera o retorno em duas execucoes [pra gerar testes melhores], com um intervalo de 5 segundos entre eles

      timer:sleep(5000),
      Qtdd1 = round(( PR#position_maintenance_request.long_qty /100)/2) * 100,
      ReportExecution1 = exec_report:build_execution_report_for_position_maintenance(PR, Qtdd1),
      exec_report_dispatcher:dispatch2(ReportExecution1),
      %?DBG("position_maintenance_request Executed 1 => ", [Qtdd1, ReportExecution1]),

      Bin1 = exec_report:report_to_fix_bin(ReportExecution1, 100),
      R1 = fix:dump(Bin1),

      Qtdd2 = PR#position_maintenance_request.long_qty - Qtdd1,
      if
        (Qtdd2 > 0) ->
          timer:sleep(10000),
          ReportExecution2 = exec_report:build_execution_report_for_position_maintenance(PR, Qtdd2),
          exec_report_dispatcher:dispatch2(ReportExecution2),
          %?DBG("position_maintenance_request Executed 2 => ", [Qtdd2, ReportExecution1]),

          Bin2 = exec_report:report_to_fix_bin(ReportExecution2, 100),
          R2 = fix:dump(Bin2)
      end
      ;

      true ->
        timer:sleep(10)  %faz nada!
  end,

  %?DBG("position_maintenance_request Finished! ", [R2]),

  NewState.
