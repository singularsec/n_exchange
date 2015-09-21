-module(fix_order_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").


handle_new_order_single(#new_order_single{} = Order, Messages, Rest, #state{} = State) ->
  % [ {msg_seq_num,9}, {sender_comp_id, <<"INIT">>},
  % {target_comp_id, <<"ACCEPT">>}, {order_qty,10}, {symbol,<<"PETR4">>}]
  NewOrder = order_from_new_order_single(Order),
  % potential bottleneck here as it's a sync call
  BookPid = nexchange_bookregistry:get_book(NewOrder#order.symbol),

  nexchange_trading_book:send_new_order_single(BookPid, NewOrder),

  fix_message_handler:handle_messages(Messages, Rest, State).


order_from_new_order_single(#new_order_single{} = Order) ->
  Fields = Order#new_order_single.fields,

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
    expirationt = Order#new_order_single.expire_time,
    expirationd = Order#new_order_single.expire_date,
    account     = Order#new_order_single.account,
    cl_ord_id   = Order#new_order_single.cl_ord_id
  }.
