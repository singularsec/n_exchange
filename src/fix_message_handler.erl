-module(fix_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

% http://www.bmfbovespa.com.br/en-us/services/trading-platforms/puma-trading-system/EntryPoint-quick-reference.asp

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
%    {msg_seq_num,9},  {sender_comp_id, <<"INIT">>}, {target_comp_id, <<"ACCEPT">>}, {order_qty,10}, {symbol,<<"PETR4">>}]}]

% new order single
%   8=FIX.4.4 | 9=209 | 35=D | 34=4325 | 49=CCLRA300 | 52=20150909-20:09:49.532 | 56=OE101 |
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

% Party roles
% 4 - CLEARING FIRM
% 5 - INVESTOR ID
% 7 - ENTERING FIRM
% 12 - EXECUTING TRADER
% 17 - CONTRA FIRM
% 28 - CUSTODIAN
% 36 - ENTERING TRADER
% 46 - FOREIGN FIRM
% 54 - SENDER LOCATION
% 55 - SESSION ID
% 76 - DESK ID
% 99 - ORIGINATING MARKET
% 1001 - ORDER ORIGINATION SESSION
% 1002 - MARKET SUPERVISOR ID
% 1003 - CUSTODY ACCOUNT
% 1004 - CUSTODY ALLOCATION TYPE
handle_messages([{#new_order_single{} = Order,_}|Messages], Rest, #state{} = State) ->
  ?DBG("new_order_single ~p", Order),

  % send to registered book

  % nexchange_bookregistry:send_to_book(Symbol, Message)

  handle_messages(Messages, Rest, State);


handle_messages([{#heartbeat{} = Hb,_}|Messages], Rest, #state{} = State) ->
  ?DBG("heartbeat ~p", Hb#heartbeat.fields),

  Ts = erlang:timestamp(),
  handle_messages(Messages, Rest, State#state{lastheartbeat=Ts});


handle_messages([{#test_request{test_req_id=ReqId} = Tr,_}|Messages], Rest, #state{} = State) ->
  ?DBG("test_request ~p", Tr),
  NewState = send(heartbeat, [{test_req_id,ReqId}], Tr#test_request.fields, State),
  handle_messages(Messages, Rest, NewState);


handle_messages([{#resend_request{begin_seq_no=_BeginSeq,end_seq_no=_EndSeq} = Msg,_}|Messages],
                Rest, #state{} = State) ->
  ?DBG("resend_request ~p", Msg),
  % send(heartbeat, [{test_req_id,ReqId}], Tr#test_request.fields, State),
  handle_messages(Messages, Rest, State);


handle_messages([{#logon{} = Logon,Bin}|Messages], Rest, #state{} = State) ->
  % {logon,<<"20150912-21:51:02.065">>,0,30,undefined,undefined,
  %          undefined,undefined,undefined,undefined,[],
  %          [{msg_seq_num,2},
  %           {sender_comp_id,<<"INIT">>},
  %           {target_comp_id,<<"ACCEPT">>}]
  % },
  ?DBG("logon ~p", [Logon#logon.fields, fix:dump(Bin)]),

  NewState = send(logon,
                  [ {reset_seq_num_flag, "Y"},
                    {encrypt_method,0},
                    {heart_bt_int, Logon#logon.heart_bt_int} ],
                  Logon#logon.fields, State),

  Sender = proplists:get_value(sender_comp_id, Logon#logon.fields),

  nexchange_fixsession_eventmgr:notify_session_authenticated(Sender, self()),

  handle_messages(Messages, Rest, NewState#state{authenticated=true, sessionid=Sender});


handle_messages([{#logout{} = Logout,_}|_], _, #state{} = State) ->
  ?DBG("logout ~p", Logout),

  % kill this process
  {stop, normal, State};


handle_messages([{Msg,_Bin}|Messages], Rest, #state{} = State) ->
  ?DBG("unhandled ~p", Msg),
  handle_messages(Messages, Rest, State);


handle_messages([], Rest, State) ->
  {noreply, State#state{prevbuffer=Rest}}.



send(MsgType, Body, Fields, #state{socket=Socket, our_seq=Seq} = State) ->
  Sender = proplists:get_value(sender_comp_id, Fields),
  Target = proplists:get_value(target_comp_id, Fields),

  ReplyBin = fix:pack(MsgType, Body, Seq, Target, Sender),

  % Reply = iolist_to_binary(ReplyBin),
  ok = gen_tcp:send(Socket, ReplyBin),

  ?DBG("wrote to socket ~p ~p", [MsgType, Seq, fix:dump(iolist_to_binary(ReplyBin))]),

  State#state{our_seq=Seq + 1}.
