
-module(quote_request_handler).

-export([handle/2, test/0]).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44_xp.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").


test() -> 
  InitialState = #state{our_seq=1, socket=undefined},
%   {quote_req_id,<<"35677_0">>},
%   {private_quote,<<"Y">>},
%   {unique_trade_id,undefined},
%   {execute_underlying_trade,<<"0">>},
%   {related_sym,[]},
%   {fields,[
						% {msg_seq_num,17},
      %      {sender_comp_id,<<"CLEAR_3">>},
      %      {target_comp_id,<<"XPOMS">>},
      %      {no_related_sym,2},
      %      {symbol,<<"PETR4T">>},
      %      {security_exchange,<<"XBSP">>},
      %      {side,buy},
      %      {cl_ord_id,<<"35678_0">>},
      %      {order_qty,100},
      %      {settl_type,regular},
      %      {days_to_settlement,<<"30">>},
      %      {fixed_rate,<<"0.012">>},
      %      {account,<<"704008">>},
      %      {transact_time,<<"20160823-20:45:17">>},
      %      {price,10},
      %      {symbol,<<"PETR4T">>},
      %      {security_exchange,<<"XBSP">>},
      %      {side,sell},
      %      {cl_ord_id,<<"35679_0">>},
      %      {order_qty,100},
      %      {settl_type,regular},
      %      {days_to_settlement,<<"30">>},
      %      {fixed_rate,<<"0.012">>},
      %      {account,<<"7003286">>},
      %      {transact_time,<<"20160823-20:45:17">>},
      %      {price,10},
%            {undefined,<<"CL_MESA">>}]}]

  QR = #quote_request{
		sending_time= <<"20160823-23:45:17.354">>, 
		quote_req_id= <<"31639280_0">>,
		private_quote= <<"Y">>,
		execute_underlying_trade= <<"0">>,
		related_sym = [],
		fields = [{msg_seq_num,17},
	            {sender_comp_id,<<"CLEAR_3">>},
	            {target_comp_id,<<"XPOMS">>},
	            {no_related_sym,2},
	            {symbol,<<"PETR4T">>},
	            {security_exchange,<<"XBSP">>},
	            {side,buy},
	            {cl_ord_id,<<"35678_0">>},
	            {order_qty,100},
	            {settl_type,regular},
	            {days_to_settlement,<<"30">>},
	            {fixed_rate,<<"0.012">>},
	            {account,<<"704008">>},
	            {transact_time,<<"20160823-20:45:17">>},
	            {price,10},
	            {symbol,<<"PETR4T">>},
	            {security_exchange,<<"XBSP">>},
	            {side,sell},
	            {cl_ord_id,<<"35679_0">>},
	            {order_qty,100},
	            {settl_type,regular},
	            {days_to_settlement,<<"30">>},
	            {fixed_rate,<<"0.012">>},
	            {account,<<"7003286">>},
	            {transact_time,<<"20160823-20:45:17">>},
	            {price,10}
	           ]
  },

  handle(QR, InitialState).


handle(#quote_request{} = QR, State) -> 
  
  Fields = QR#quote_request.fields,
  Legs = fix_utils:extract_quote_request_legs(Fields),

  NewState = confirm_and_execute(QR, Legs, State),

  NewState.


 % [{sending_time,<<"20160115-20:17:10.628">>},
 %  {quote_req_id,<<"31568_0">>},
 %  {private_quote,<<"Y">>},
 %  {unique_trade_id,undefined},
 %  {execute_underlying_trade,<<"0">>},
 %  {related_sym,[]},
 %  {fields,[{msg_seq_num,712},
 %           {sender_comp_id,<<"CLEAR">>},
 %           {target_comp_id,<<"XPOMS">>},
confirm_and_execute(QR, [#quote_request_leg{} = Leg | Rest], #state{} = State) ->
  QuoteId = list_to_binary ( integer_to_list( State#state.our_seq ) ),
  PrimaryFields = [
    {account, Leg#quote_request_leg.account },
    {order_qty, Leg#quote_request_leg.order_qty },
    {price, Leg#quote_request_leg.price },
    {side, Leg#quote_request_leg.side },
    {symbol, Leg#quote_request_leg.symbol },
    {transact_time, Leg#quote_request_leg.transact_time },
    {settl_type, Leg#quote_request_leg.settl_type },
    {quote_id, QuoteId}, % unique
    {quote_req_id, QR#quote_request.quote_req_id },
    {quote_status, 0 },
    {private_quote, QR#quote_request.private_quote },
    {days_to_settlement, Leg#quote_request_leg.days_to_settlement },
    {fixed_rate, Leg#quote_request_leg.fixed_rate },
    {execute_underlying_trade, QR#quote_request.execute_underlying_trade },
    {quote_status_report_type, 1 },
    {quote_status_response_to, 1 }
  ],
  FromSessId = proplists:get_value(sender_comp_id, QR#quote_request.fields),
  DestSessId = proplists:get_value(target_comp_id, QR#quote_request.fields),
  Fields = [{cl_ord_id, Leg#quote_request_leg.cl_ord_id},
            {target_comp_id, DestSessId},
            {sender_comp_id, FromSessId}],

  NewState = fix_message_handler:send(quote_status_report, PrimaryFields, Fields, State), % AI

  ReportNew = exec_report:build_accept_for_quote_request_leg(QR, Leg, QuoteId),
  exec_report_dispatcher:dispatch2(ReportNew),
  Bin1 = exec_report:report_to_fix_bin(ReportNew, 100),
  R1 = fix0:dump(Bin1),
  % ?DBG("New ~p~n", [R1]),

  ReportFilled = exec_report:build_filled_for_quote_request_leg(QR, Leg, QuoteId),
  exec_report_dispatcher:dispatch2(ReportFilled),
  Bin2 = exec_report:report_to_fix_bin(ReportFilled, 100),
  R2 = fix0:dump(Bin2),
  % ?DBG("Filled ~p~n", [R2]),

  confirm_and_execute(QR, Rest, NewState);



confirm_and_execute(_, [], State) -> State.

