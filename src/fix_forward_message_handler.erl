-module(fix_forward_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44_xp.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").



% [{sending_time,<<"20160115-20:17:10.628">>},
%  {quote_req_id,<<"31568_0">>},
%  {rfq_req_id,undefined},
%  {cl_ord_id,<<"31570_0">>},
%  {order_capacity,undefined},
%  {prev_close_px,undefined},
%  {quote_request_type,undefined},
%  {quote_type,undefined},             {trading_session_id,undefined},
%  {trading_session_sub_id,undefined}, {trade_origination_date,undefined},
%  {side,sell},
%  {qty_type,undefined},
%  {settl_type,regular},
%  {settl_date,undefined},
%  {settl_date2,undefined},
%  {order_qty2,undefined},
%  {currency,undefined},
%  {account,<<"4008">>},
%  {acct_id_source,undefined},
%  {account_type,undefined},
%  {quote_price_type,undefined},
%  {ord_type,undefined},
%  {valid_until_time,undefined},
%  {expire_time,undefined},
%  {transact_time,<<"20160115-18:17:10">>},
%  {price_type,undefined},
%  {price,16.192},
%  {price2,undefined},
%  {text,undefined},
%  {encoded_text,undefined},
%  {related_sym,[]},
%  {legs,[]},
%  {quote_qualifiers,[]},
%  {fields,[{msg_seq_num,712},
%           {sender_comp_id,<<"CLEAR">>},
%           {target_comp_id,<<"XPOMS">>},
%           {no_related_sym,2},
%           {symbol,<<"PETR4T">>},
%           {security_exchange,<<"XBSP">>},
%           {order_qty,100},
%           {5497,<<"20">>},
%           {5706,<<"0.012">>},
%           {symbol,<<"PETR4T">>},
%           {security_exchange,<<"XBSP">>},
%           {order_qty,100},
%           {5497,<<"20">>},
%           {5706,<<"0.012">>},
%           {1171,<<"Y">>},
%           {35004,<<"0">>}]}]
handle_quote_request(#quote_request{} = QR, Messages, Rest, #state{} = State) ->

    % send a quote_status_report for each leg and a exec report for each leg

    Fields = QR#quote_request.fields,
    Legs = fix_utils:extract_quote_request_legs(Fields),





    ok.
