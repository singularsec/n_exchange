-module(fix_option_execution_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44_xp.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").

handle_option_execution(#position_maintenance_request{} = QR, Messages, Rest, #state{} = State) ->

    % send a Position Maintenance Report and a exec report position

    NewState = option_execution_handler:handle(QR, State),

    fix_message_handler:handle_messages(Messages, Rest, NewState).
