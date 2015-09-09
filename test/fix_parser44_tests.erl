
-module(fix_parser44_tests).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

-include_lib("eunit/include/eunit.hrl").


hearbeat_encoding_and_decoding_test() ->
  Bin = fix:pack(heartbeat, [#heartbeat{},{sending_time,"1234"}], 2082, "CCLRA300", "OE101"),

  {ok,#heartbeat{sending_time=Sending,fields=Fields} = HB,_Rest,_C} = fix:decode( iolist_to_binary(Bin) ),

  [{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}] = Fields,

  ?assertEqual(<<"1234">>, Sending),
  ?assertEqual(<<"CCLRA300">>, Sender),
  ?assertEqual(<<"OE101">>, Target),
  ?assertEqual(2082, SeqNum),
  % error_logger:info_msg("Encoded message is ~n ~p ~n ~p ~n", [HB, Fields]),

  ok.
