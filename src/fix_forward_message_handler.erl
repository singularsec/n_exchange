-module(fix_forward_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").




handle_quote_request(#order_cancel_request{} = Order, Messages, Rest, #state{} = State) ->

    % send a quote_status_report for each leg and a exec report for each leg

    

    ok.
