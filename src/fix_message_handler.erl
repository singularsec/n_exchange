-module(fix_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

% http://www.bmfbovespa.com.br/en-us/services/trading-platforms/puma-trading-system/EntryPoint-quick-reference.asp

handle_messages([{#new_order_single{} = Order,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("new_order_single ~n ~p", fix:crack(Order)),
  fix_order_message_handler:handle_new_order_single(Order, Messages, Rest, State);

handle_messages([{#new_order_cross{} = Order,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("new_order_single ~n ~p", fix:crack(Order)),
  fix_order_cross_message_handler:handle_new_order_cross(Order, Messages, Rest, State);

handle_messages([{#order_cancel_request{} = CR,_}|Messages], Rest, #state{} = State) ->
  ?DBG("order_cancel_request ~n ~p", fix:crack(CR)),
  fix_order_message_handler:handle_order_cancel_request(CR, Messages, Rest, State);

% order_cancel_replace_request

handle_messages([{#order_cancel_replace_request{} = CR,_}|Messages], Rest, #state{} = State) ->
  ?DBG("order_cancel_replace_request ~n ~p", fix:crack(CR)),
  fix_order_message_handler:handle_order_cancel_replace_request(CR, Messages, Rest, State);

handle_messages([{#quote_request{} = CR,_}|Messages], Rest, #state{} = State) ->
  ?DBG("quote_request ~n ~p ~n", fix:crack(CR)),
  fix_forward_message_handler:handle_quote_request(CR, Messages, Rest, State);

handle_messages([{#heartbeat{} = _Hb,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("heartbeat ~p", _Hb#heartbeat.fields),
  Ts = erlang:timestamp(),
  handle_messages(Messages, Rest, State#state{lastheartbeat=Ts});

handle_messages([{#test_request{test_req_id=ReqId} = Tr,_}|Messages], Rest, #state{} = State) ->
  % ?DBG("test_request ~p", Tr),
  ?DBG("resend_request ~p", Tr),
  NewState = send(heartbeat, [{test_req_id,ReqId}], Tr#test_request.fields, State),
  handle_messages(Messages, Rest, NewState);

handle_messages([{#resend_request{begin_seq_no=_BeginSeq,end_seq_no=_EndSeq} = Msg,_}|Messages],
                Rest, #state{} = State) ->
  ?DBG("resend_request ~p", Msg),
  % send(heartbeat, [{test_req_id,ReqId}], Tr#test_request.fields, State),
  handle_messages(Messages, Rest, State);

handle_messages([{#logon{} = Logon,Bin}|Messages], Rest, #state{} = State) ->

  fix:crack(Logon),
  ?DBG("logon bin ~n~p~n", [Bin]),

  Sender   = proplists:get_value(sender_comp_id, Logon#logon.fields),
  TheirSeq = proplists:get_value(msg_seq_num, Logon#logon.fields),
  % RequestToReset = Logon#logon.reset_seq_num_flag == true,
  HeartBtInterval = Logon#logon.heart_bt_int,
  AwaitingReply = State#state.sentlogonrequest,

  {ShouldResetSeq, IsReset} =
    if TheirSeq /= 1 ->
         % received a "reset" kind of logon message
         { [{reset_seq_num_flag, "Y"}], true };
       true ->
         % this will actually force the initiator to re-logon with seq = 1
         { [], false }
    end,

  MaybeNewState =
    if
      IsReset -> State#state{sentlogonrequest=true};
      true -> State
    end,

  Msg = {'$gen_cast', {send_heartbeat, Logon#logon.fields}},
  {ok,TimerRef} = timer:send_interval(HeartBtInterval * 1000, Msg),
  nexchange_fixsession_eventmgr:notify_session_authenticated(Sender, self()),
  NewState = MaybeNewState#state{timer_ref=TimerRef, their_seq=TheirSeq},

  % ?DBG("logon ~n ~p Reply ~p ~n ~p ~n", [Logon#logon.fields, {ShouldResetSeq,NewState,IsReset}, fix:dump(Bin)]),
  % ?DBG("logon received raw ~n~p~nIs reset? ~p~n",[fix:dump(Bin), IsReset]),

  NewState2 =
    if
      AwaitingReply == false ->
        send(logon,
             ShouldResetSeq ++ [{encrypt_method, 0},
                                {heart_bt_int, Logon#logon.heart_bt_int}],
             Logon#logon.fields, NewState);
      true -> NewState#state{sentlogonrequest=false}
    end,

  handle_messages(Messages, Rest, NewState2#state{authenticated=true, sessionid=Sender});


handle_messages([{#logout{} = Logout,_}|_], _, #state{socket=Socket} = State) ->
  ?DBG("logout ~p", Logout),

  gen_tcp:close(Socket),

  % kill this process
  {stop, normal, State};

handle_messages([{#position_maintenance_request{} = PR,_}|Messages], Rest, #state{} = State) ->
  option_execution_handler:handle(PR, Messages, Rest, State);

handle_messages([{Msg,_Bin}|Messages], Rest, #state{} = State) ->
  ?DBG("unhandled ~n~p~n~p~n~p~n~p~n", Msg),
  handle_messages(Messages, Rest, State);

handle_messages([], Rest, State) ->
  {noreply, State#state{prevbuffer=Rest}}.

send(MsgType, Body, Fields, #state{socket=Socket, our_seq=Seq} = State) ->
  Sender = proplists:get_value(sender_comp_id, Fields),
  Target = proplists:get_value(target_comp_id, Fields),

  ReplyBin = fix:pack(MsgType, Body, Seq, Target, Sender),

  % Reply = iolist_to_binary(ReplyBin),
  if 
    Socket =:= undefined -> 
      ?DBG("wrote to socket ~n~p~n~p~n~p~n", [MsgType, Seq, fix:dump(iolist_to_binary(ReplyBin))]);
    true ->
      ?DBG("wrote to socket ~n~p~n", [fix:dump(iolist_to_binary(ReplyBin))]),
      ok = gen_tcp:send(Socket, ReplyBin)
  end,

  %?DBG("wrote to socket ~n~p~n~p~n~p~n", [MsgType, Seq, fix:dump(iolist_to_binary(ReplyBin))]),

  State#state{our_seq=Seq + 1}.
