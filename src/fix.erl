
-module(fix).

-compile(export_all).

-include("log.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

-type fix_message() :: any().


record_to_proplist(#new_order_single{} = Rec) ->
  lists:zip(record_info(fields, new_order_single), tl(tuple_to_list(Rec)));

record_to_proplist(#logon{} = Rec) ->
  lists:zip(record_info(fields, logon), tl(tuple_to_list(Rec))).

crack(#new_order_single{} = NOS) ->
  Prop = record_to_proplist(NOS),
  ?DBG("new_order_single ~n ~p ~n", [Prop]);

crack(#order_cancel_request{} = NOS) ->
  Prop = record_to_proplist(NOS),
  ?DBG("order_cancel_request ~n ~p ~n", [Prop]);

crack(#logon{} = NOS) ->
  Prop = record_to_proplist(NOS),
  ?DBG("logon ~n ~p ~n", [Prop]).


%% @doc packs fix message into binary
-spec pack(atom(), list(), non_neg_integer(), any(), any()) -> iolist().
pack(MessageType, Body, SeqNum, Sender, Target)
     when MessageType =/= undefined, is_list(Body), is_integer(SeqNum), Sender =/= undefined, Target =/= undefined ->

  Header2 = [{msg_type, MessageType},{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}
  % ,{poss_dup_flag, "N"}
  ] ++ case proplists:get_value(sending_time, Body) of
    undefined -> [{sending_time, fix:now()}];
    _ -> []
  end,
  Body1 = encode(Header2 ++ Body),
  BodyLength = iolist_size(Body1),
  Body2 = iolist_to_binary([encode([{begin_string, "FIX.4.4"}, {body_length, BodyLength}]), Body1]),
  CheckSum = checksum(Body2),
  Body3 = [Body2, encode([{check_sum, CheckSum}])],
  % ?D({out,Header2, dump(Body3)}),
  Body3.

-spec decode(binary()) -> {ok, fix_message(), binary(), binary()} | {more, non_neg_integer()} | error.
decode(Bin) ->
  try decode0(Bin) of
    Result -> Result
  catch
    error:Error ->
      ?DBG("Failed to decode fix '~s': ~p~n~p~n", [fix:dump(Bin), Error, erlang:get_stacktrace()]),
      error(invalid_fix)
  end.

decode0(Bin) ->
  case decode_fields(Bin) of
    {ok, Fields, MessageBin, Rest} ->
      % {ok, fix_group:postprocess(fix_parser:decode_message(Fields)), MessageBin, Rest};
      {ok, fix_parser44:decode_message(Fields), MessageBin, Rest};
    Else ->
      Else
  end.

decode_fields(<<"8=FIX.4.4",1,"9=", Bin/binary>> = FullBin) ->
  case binary:split(Bin, <<1>>) of
    [BinLen, Rest1] ->
      BodyLength = list_to_integer(binary_to_list(BinLen)),
      case Rest1 of
        <<Message:BodyLength/binary, "10=", _CheckSum:3/binary, 1, Rest2/binary>> ->
          MessageLength = size(FullBin) - size(Rest2),
          <<MessageBin:MessageLength/binary, _/binary>> = FullBin,
          {ok, fix_splitter44:split(Message), MessageBin, Rest2};
        _ ->
          {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
      end;
    _ ->
      {more, 1}
  end;

decode_fields(<<"8", Rest/binary>>) when length(Rest) < 14 ->
  {more, 14 - size(Rest)};

decode_fields(<<"8", _/binary>>) ->
  {more, 1};

decode_fields(<<>>) ->
  {more, 14};

decode_fields(<<_/binary>>) ->
  error.

checksum(Packet) ->
  lists:flatten(io_lib:format("~3..0B", [lists:sum([Char || <<Char>> <=iolist_to_binary(encode(Packet))]) rem 256])).

encode(Packet) when is_binary(Packet) -> Packet;
encode([{_K,_V}|_] = Packet) ->
  [[fix_parser44:number_by_field(Key), "=", fix_parser44:encode_typed_field(Key, Value), 1] || {Key, Value} <- Packet].

encode_value(Value) when is_number(Value) -> integer_to_list(Value);
encode_value(Value) when is_float(Value) -> io_lib:format("~.2f", [Value]);
encode_value(Value) when is_list(Value) -> Value;
encode_value(Value) when is_binary(Value) -> Value.


dump(Bin) ->
  re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).

-spec now() -> string().
now() ->
  {_Mega, _Sec, Micro} = erlang:timestamp(),
  Milli = Micro div 1000,
  {{YY,MM,DD}, {H,M,S}} = calendar:universal_time(),
  % timestamp(to_date_ms( Ts )).
  timestamp( {{YY,MM,DD}, {H,M,S,Milli}} ).

timestamp({{YY,MM,DD},{H,M,S,Milli}}) ->
  % 20120529-10:40:17.578
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B.~3..0B", [YY, MM, DD, H, M, S, Milli])).

to_date_ms({Mega, Sec, Micro}) ->
  Seconds = Mega*1000000 + Sec,
  Milli = Micro div 1000,
  {Date, {H,M,S}} = calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
  {Date, {H,M,S,Milli}}.
