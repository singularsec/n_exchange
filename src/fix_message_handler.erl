-module(fix_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").


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

handle_messages([{#new_order_single{} = Order,_}|Messages], Rest, #state{} = State) ->
  ?DBG("new_order_single ~p", Order),

  % send to registered book

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

  handle_messages(Messages, Rest, NewState#state{authenticated=true});


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
