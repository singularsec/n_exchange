-module(fix_order_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44_xp.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").

handle_order_cancel_request(#order_cancel_request{} = Req, Messages, Rest, #state{} = State) ->
  
  CancelOrder = order_cancel_from_cancel_order_request(Req),

  BookPid = nexchange_bookregistry:get_book(CancelOrder#order_cancel.symbol),
  
  Succeeded = nexchange_trading_book:try_cancel(BookPid, CancelOrder) == done,

  if 
    Succeeded == false -> 
      % means no book cancelled the order
      Report = exec_report:build_cancel_reject(CancelOrder, "order not found"),
      exec_report_dispatcher:dispatch(Report);
    true -> 
       ok
      % exec report sent by the book (indirectly)
  end,

  fix_message_handler:handle_messages(Messages, Rest, State).


handle_new_order_single(#new_order_single{} = Order, Messages, Rest, #state{} = State) ->
  % [ {msg_seq_num,9}, {sender_comp_id, <<"INIT">>},
  % {target_comp_id, <<"ACCEPT">>}, {order_qty,10}, {symbol,<<"PETR4">>}]
  NewOrder = order_from_new_order_single(Order),

  % TODO: magic price should cause a reject

  BookPid = nexchange_bookregistry:get_book(NewOrder#order.symbol),

  nexchange_trading_book:send_new_order_single(BookPid, NewOrder),

  fix_message_handler:handle_messages(Messages, Rest, State).


handle_order_cancel_replace_request(#order_cancel_replace_request{} = Order, Messages, Rest, #state{} = State) ->

  ModifyOrder = order_from_order_cancel_replace_request(Order),

  BookPid = nexchange_bookregistry:get_book(ModifyOrder#order_modify.symbol),

  nexchange_trading_book:try_modify_order(BookPid, ModifyOrder),

  fix_message_handler:handle_messages(Messages, Rest, State).


order_from_new_order_single(#new_order_single{} = Order) ->

  Fields = Order#new_order_single.fields,
  Parties = fix_utils:extract_parties(Fields),

  #order{
    to_sessionid   = binary_to_list( proplists:get_value(target_comp_id, Fields) ),
    from_sessionid = binary_to_list( proplists:get_value(sender_comp_id, Fields) ),
    id          = integer_to_list( erlang:unique_integer([positive]) ),
    symbol      = binary_to_list( proplists:get_value(symbol, Fields) ),
    qtd         = proplists:get_value(order_qty, Fields),
    order_type  = Order#new_order_single.ord_type,
    side        = Order#new_order_single.side,
    price       = Order#new_order_single.price,
    stop_price  = Order#new_order_single.stop_px,
    price_type  = Order#new_order_single.price_type,
    time        = Order#new_order_single.sending_time,
    timeinforce = Order#new_order_single.time_in_force,
    % expirationt = Order#new_order_single.expire_time,
    expirationd = Order#new_order_single.expire_date,
    account     = Order#new_order_single.account,
    cl_ord_id   = Order#new_order_single.cl_ord_id,
    parties     = Parties
  }.


order_cancel_from_cancel_order_request(#order_cancel_request{} = Req) -> 
  
  Fields = Req#order_cancel_request.fields,
  Parties = fix_utils:extract_parties(Fields),

  #order_cancel{
    to_sessionid   = binary_to_list( proplists:get_value(target_comp_id, Fields) ),
    from_sessionid = binary_to_list( proplists:get_value(sender_comp_id, Fields) ),
    symbol         = binary_to_list( proplists:get_value(symbol, Fields) ),
    order_id       = Req#order_cancel_request.order_id,
    cl_ord_id      = Req#order_cancel_request.cl_ord_id,
    orig_cl_ord_id = Req#order_cancel_request.orig_cl_ord_id,
    side           = Req#order_cancel_request.side, 
    account        = Req#order_cancel_request.account,
    parties        = Parties
  }.


order_from_order_cancel_replace_request(#order_cancel_replace_request{} = Req) -> 
  Fields = Req#order_cancel_replace_request.fields,
  Parties = fix_utils:extract_parties(Fields),

  #order_modify{
    to_sessionid   = binary_to_list( proplists:get_value(target_comp_id, Fields) ),
    from_sessionid = binary_to_list( proplists:get_value(sender_comp_id, Fields) ),
    symbol         = binary_to_list( proplists:get_value(symbol, Fields) ),
    order_qty      = proplists:get_value(order_qty, Fields),
    order_id       = Req#order_cancel_replace_request.order_id,
    cl_ord_id      = Req#order_cancel_replace_request.cl_ord_id,
    orig_cl_ord_id = Req#order_cancel_replace_request.orig_cl_ord_id,
    price          = Req#order_cancel_replace_request.price, 
    side           = Req#order_cancel_replace_request.side, 
    account        = Req#order_cancel_replace_request.account,
    parties        = Parties
  }.