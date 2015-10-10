% -file("src/receiver_pb2.erl", 1).

-module(receiver_pb2).

-include("../include/receiver_pb.hrl").

-export([encode_pricechangeacksimplemessage/1,
	 decode_pricechangeacksimplemessage/1,
	 delimited_decode_pricechangeacksimplemessage/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-export([int_to_enum/2, enum_to_int/2]).

% -record(pricechangeacksimplemessage,
% 	{refid, price, quantity, buyer, seller, direction,
% 	 netchgprevday, variation, volume, condition, symbol}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_pricechangeacksimplemessage(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_pricechangeacksimplemessage(Record)
    when is_record(Record, pricechangeacksimplemessage) ->
    encode(pricechangeacksimplemessage, Record).

encode(pricechangeacksimplemessage, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(pricechangeacksimplemessage, Record) ->
    [iolist(pricechangeacksimplemessage, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(pricechangeacksimplemessage, Record) ->
    [pack(1, optional,
	  with_default(Record#pricechangeacksimplemessage.refid,
		       none),
	  string, []),
     pack(2, optional,
	  with_default(Record#pricechangeacksimplemessage.price,
		       none),
	  float, []),
     pack(3, optional,
	  with_default(Record#pricechangeacksimplemessage.quantity,
		       none),
	  float, []),
     pack(4, optional,
	  with_default(Record#pricechangeacksimplemessage.buyer,
		       none),
	  string, []),
     pack(5, optional,
	  with_default(Record#pricechangeacksimplemessage.seller,
		       none),
	  string, []),
     pack(6, optional,
	  with_default(Record#pricechangeacksimplemessage.direction,
		       none),
	  pricechangeacksimplemessage_tickdirection, []),
     pack(7, optional,
	  with_default(Record#pricechangeacksimplemessage.netchgprevday,
		       none),
	  float, []),
     pack(8, optional,
	  with_default(Record#pricechangeacksimplemessage.variation,
		       none),
	  float, []),
     pack(9, optional,
	  with_default(Record#pricechangeacksimplemessage.volume,
		       none),
	  float, []),
     pack(10, optional,
	  with_default(Record#pricechangeacksimplemessage.condition,
		       none),
	  string, []),
     pack(11, optional,
	  with_default(Record#pricechangeacksimplemessage.symbol,
		       none),
	  string, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(pricechangeacksimplemessage_tickdirection,
	    'Down') ->
    2;
enum_to_int(pricechangeacksimplemessage_tickdirection,
	    'Up') ->
    1;
enum_to_int(pricechangeacksimplemessage_tickdirection,
	    'None') ->
    0.

int_to_enum(pricechangeacksimplemessage_tickdirection,
	    2) ->
    'Down';
int_to_enum(pricechangeacksimplemessage_tickdirection,
	    1) ->
    'Up';
int_to_enum(pricechangeacksimplemessage_tickdirection,
	    0) ->
    'None';
int_to_enum(_, Val) -> Val.

decode_pricechangeacksimplemessage(Bytes)
    when is_binary(Bytes) ->
    decode(pricechangeacksimplemessage, Bytes).

delimited_decode_pricechangeacksimplemessage(Bytes) ->
    delimited_decode(pricechangeacksimplemessage, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(pricechangeacksimplemessage, Bytes)
    when is_binary(Bytes) ->
    Types = [{11, symbol, string, []},
	     {10, condition, string, []}, {9, volume, float, []},
	     {8, variation, float, []},
	     {7, netchgprevday, float, []},
	     {6, direction,
	      pricechangeacksimplemessage_tickdirection, []},
	     {5, seller, string, []}, {4, buyer, string, []},
	     {3, quantity, float, []}, {2, price, float, []},
	     {1, refid, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pricechangeacksimplemessage, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(pricechangeacksimplemessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pricechangeacksimplemessage),
						   Record, Name, Val)
			  end,
			  #pricechangeacksimplemessage{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.
