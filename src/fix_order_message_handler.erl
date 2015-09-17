-module(fix_order_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

% {new_order_single, <<"20150913-06:31:21.719">>,  <<"x1">>,undefined,
%    undefined,    undefined,   undefined,   <<"xxx1">>,
%    undefined,   undefined,   undefined,   undefined,    undefined,   undefined,   undefined,   undefined,
%    undefined,   undefined,   undefined,   undefined,    undefined,   undefined,   undefined,   undefined,
%    undefined,buy,   undefined,   <<"20150913-03:28:51.345">>,
%    undefined,limit,   undefined, undefined,    undefined,    undefined,    undefined,
%    undefined,   undefined,   undefined,   undefined,    undefined,   undefined,   undefined,   undefined,
%    undefined,   undefined,   undefined,   undefined,   undefined,   undefined,   undefined,   undefined,
%    undefined,   undefined,   undefined,   undefined,   undefined,   undefined,   undefined,    undefined,
%    undefined,   undefined,   undefined,   undefined, undefined,[],[],[], [
%    {msg_seq_num,9},  {sender_comp_id, <<"INIT">>}, {target_comp_id, <<"ACCEPT">>},
%    {order_qty,10}, {symbol,<<"PETR4">>}]}]

% new order single
%    1=6216 |                <---- Account
%    11=49803_0 |            <---- ClOrdID
%    38=100 |                <---- OrderQty
%    40=2 |                  <---- OrdType
%    44=1 |                  <---- Price
%    54=1 |                  <---- Side  1 = Buy  2 = Sell
%    55=PETRI5 |             <---- Symbol
%    59=0 |                  <---- TimeInForce  0 = Day
%    60=20150909-20:09:49 |  <---- TransactTime
%    453=3 |                 <---- NoPartyIDs
%      448=CCLRA300 |          <---- PartyID
%      447=D |                 <---- PartyIDSource  D= Proprietary/Custom code
%      452=36 |                <---- PartyRole  36 = Entering Trader
%    448=308 |               <---- PartyID
%    447=D |                 <---- PartyIDSource
%    452=7 |                 <---- 7 = Contra Firm
%      448=DMA1 |              <---- PartyID
%      447=D |                 <---- PartyIDSource
%      452=54 |                <---- CUSTOM sender location

handle_new_order_single(#new_order_single{} = Order, Messages, Rest, #state{} = State) ->
  IsOrderValid = case Order#new_order_single.ord_type of
    market -> true;
    limit -> true;
    stop -> true;
    stoplimit -> true;
    marketwithleftoverlimit -> true;
    _ -> false
  end,
  handle_new_order_single(Order, IsOrderValid, Messages, Rest, State).


handle_new_order_single(#new_order_single{} = Order, false, Messages, Rest, #state{} = State) ->
  % TODO: send reject due to invalid/not supported order type
  fix_message_handler:handle_messages(Messages, Rest, State);


handle_new_order_single(#new_order_single{} = Order, true, Messages, Rest, #state{} = State) ->
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
