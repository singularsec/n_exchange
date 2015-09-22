-module(fix_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

% http://www.bmfbovespa.com.br/en-us/services/trading-platforms/puma-trading-system/EntryPoint-quick-reference.asp

handle_messages([{#new_order_single{} = Order,_}|Messages], Rest, #state{} = State) ->
  ?DBG("new_order_single ~n ~p", fix:crack(Order)),
  fix_order_message_handler:handle_new_order_single(Order, Messages, Rest, State);


handle_messages([{#heartbeat{} = Hb,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("heartbeat ~p", Hb#heartbeat.fields),
  Ts = erlang:timestamp(),
  handle_messages(Messages, Rest, State#state{lastheartbeat=Ts});


handle_messages([{#test_request{test_req_id=ReqId} = Tr,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("test_request ~p", Tr),
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

  Sender = proplists:get_value(sender_comp_id, Logon#logon.fields),
  Seq = proplists:get_value(msg_seq_num, Logon#logon.fields),
  ReqReset = Logon#logon.reset_seq_num_flag == <<"Y">>,

  ShouldResetSeq =
    if Seq == 1 ->
        % expected valid session, so we register it
        nexchange_fixsession_eventmgr:notify_session_authenticated(Sender, self()),
        [];
      % else, we ask for a new logon
      true -> [{reset_seq_num_flag, "Y"}]
    end,

  NewState =
    if ReqReset == true -> State#state{our_seq=1};
       true -> State
    end,

  ?DBG("logon ~n ~p Reply ~p ~n ~p ~n", [Logon#logon.fields, ShouldResetSeq, fix:dump(Bin)]),

  NewState2 = send(logon,
                  ShouldResetSeq ++ [ {encrypt_method,0}, {heart_bt_int, Logon#logon.heart_bt_int} ],
                  Logon#logon.fields, NewState),

  handle_messages(Messages, Rest, NewState2#state{authenticated=true, sessionid=Sender});


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

  ?DBG("wrote to socket ~n ~p ~n ~p ~n ~p ~n", [MsgType, Seq, fix:dump(iolist_to_binary(ReplyBin))]),

  State#state{our_seq=Seq + 1}.
