-module(fix_forward_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").




handle_quote_request(#quote_request{} = QR, Messages, Rest, #state{} = State) ->

    % send a quote_status_report for each leg and a exec report for each leg

    NewState = quote_request_handler:handle(QR, State),

    fix_message_handler:handle_messages(Messages, Rest, NewState).
