-module(fix_message_handler).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

% [[{
%       {logon,<<"20150912-21:51:02.065">>,0,30,undefined,undefined,
%                undefined,undefined,undefined,undefined,[],
%                [{msg_seq_num,2},
%                 {sender_comp_id,<<"INIT">>},
%                 {target_comp_id,<<"ACCEPT">>}]
%       },
%      <<56,61,70,73,88,46,52,46,52,1,57,61,54,53,1,51,53,61,65,1,51,
%        52,61,50,1,52,57,61,73,78,73,84,1,53,50,61,50,48,49,53,48,
%        57,49,50,45,50,49,58,53,49,58,48,50,46,48,54,53,1,53,54,61,
%        65,67,67,69,80,84,1,57,56,61,48,1,49,48,56,61,51,48,1,49,48,
%        61,50,50,52,1>>
%     }
%   ],
%  <<>>, % Rest
%  {state,
%      [#Port<0.763>],
%      false,undefined,undefined,undefined,undefined,
%      <<56,61,70,73,88,46,52,46,52,1,57,61,54,53,1,51,53,61,65,1,
%        51,52,61,50,1,52,57,61,73,78,73,84,1,53,50,61,50,48,49,53,
%        48,57,49,50,45,50,49,58,53,49,58,48,50,46,48,54,53,1,53,
%        54,61,65,67,67,69,80,84,1,57,56,61,48,1,49,48,56,61,51,48,
%        1,49,48,61,50,50,52,1>>
% }]

handle_messages([{#heartbeat{} = Hb,_}|Messages], Rest, #state{} = State) ->
  ?DBG("heartbeat ~p", Hb#heartbeat.fields),

  Ts = erlang:timestamp(),
  handle_messages(Messages, Rest, State#state{lastheartbeat=Ts});


handle_messages([{#logon{} = Logon,_}|Messages], Rest, #state{socket=Socket} = State) ->
  % pack(MessageType, Body, SeqNum, Sender, Target)
  % -record(logon, {
  %   sending_time,
  %   encrypt_method,
  %   heart_bt_int,
  %   raw_data,
  %   reset_seq_num_flag,
  %   next_expected_msg_seq_num,
  %   test_message_indicator,
  %   username,
  %   password,
  %   msg_types = [],
  %   fields = []
  % }).
  % {logon,<<"20150912-21:51:02.065">>,0,30,undefined,undefined,
  %          undefined,undefined,undefined,undefined,[],
  %          [{msg_seq_num,2},
  %           {sender_comp_id,<<"INIT">>},
  %           {target_comp_id,<<"ACCEPT">>}]
  % },
  ?DBG("logon ~p", Logon#logon.fields),

  Sender = proplists:get_value(sender_comp_id, Logon#logon.fields),
  Target = proplists:get_value(target_comp_id, Logon#logon.fields),
  NewSeq = State#state.seq + 1,
  Msg = #logon{encrypt_method=0,heart_bt_int=Logon#logon.heart_bt_int},
  ReplyBin = fix:pack(logon, [Msg], NewSeq, Target, Sender),

  % iolist_to_binary
  ok = gen_tcp:send(Socket, iolist_to_binary(ReplyBin)),

  ?DBG("wrote to socket ~p ~p", [Socket, ReplyBin]),

  handle_messages(Messages, Rest, State#state{seq=NewSeq, authenticated=true});



handle_messages([{#logout{} = Logout,_}|Messages], Rest, #state{} = State) ->
  ?DBG("logout ~p", Logout#logout.fields),

  % kill this process
  exit(normal),

  handle_messages(Messages, Rest, State#state{authenticated=false});



handle_messages([], Rest, State) ->
  {noreply, State#state{prevbuffer=Rest}}.
