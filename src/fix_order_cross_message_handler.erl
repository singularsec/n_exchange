
-module(fix_order_cross_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

handle_new_order_cross(#new_order_cross{} = NewOrderCross, Messages, Rest, #state{} = State) ->

  fix:crack(NewOrderCross),

  Fields  = NewOrderCross#new_order_cross.fields,

  Sides = fix_utils:extract_new_order_cross_sides(Fields),
  ?DBG("handle_new_order_cross Sides ~n ~p ~n", [Sides]),

  NewState = confirm_and_execute(NewOrderCross, Sides, State),

  fix_message_handler:handle_messages(Messages, Rest, NewState).




confirm_and_execute(NOC, [#order_cross_side{} = Side | Rest], #state{} = State) ->
  Symbol = proplists:get_value(symbol, NOC#new_order_cross.fields),

  Reports = exec_report:build_reports_for_new_cross_order(NOC, Symbol, Side),
  
  exec_report_dispatcher:dispatch_all(Reports),

  confirm_and_execute(NOC, Rest, State);


confirm_and_execute(_, [], State) -> 
  State.
