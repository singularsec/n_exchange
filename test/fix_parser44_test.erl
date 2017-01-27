
-module(fix_parser44_test).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

-include_lib("eunit/include/eunit.hrl").


% hearbeat_encoding_and_decoding_test() ->
%   Bin = fix:pack(heartbeat, [{sending_time,"1234"}], 2082, "CCLRA300", "OE101"),
%
%   {ok,#heartbeat{sending_time=Sending,fields=Fields} = HB,_Rest,_C} = fix:decode( iolist_to_binary(Bin) ),
%
%   [{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}] = Fields,
%
%   ?assertEqual(<<"1234">>, Sending),
%   ?assertEqual(<<"CCLRA300">>, Sender),
%   ?assertEqual(<<"OE101">>, Target),
%   ?assertEqual(2082, SeqNum),
%   % error_logger:info_msg("Encoded message is ~n ~p ~n ~p ~n", [HB, Fields]),
%
%   ok.
%
% logon_encoding_and_decoding_test() ->
%
%   % 8=FIX.4.4 | 9=91 | 35=A | 34=1 | 49=CCLRA301 | 52=20150922-23:03:19.207 | 56=OE101 | 95=6 | 96=YWEKNJ | 98=0 | 108=20 | 35002=0 | 10=237 |
%   % 8=FIX.4.4 | 9=74 | 35=A | 49=OE101 | 56=CCLRA301 | 34=1 | 52=20150922-23:03:21.753 | 141=Y | 98=0 | 108=20 | 10=075 |
%
%     Bin = <<56,61,70,73,88,46,52,46,52,1,57,61,57,49,1,51,53,61,65,1,51,52,61,49,1,52,57,
%   61,67,67,76,82,65,51,48,49,1,53,50,61,50,48,49,53,48,57,50,50,45,50,51,58,48,
%   51,58,49,57,46,50,48,55,1,53,54,61,79,69,49,48,49,1,57,53,61,54,1,57,54,61,
%   89,87,69,75,78,74,1,57,56,61,48,1,49,48,56,61,50,48,1,51,53,48,48,50,61,48,1,
%   49,48,61,50,51,55,1>>,
%     % Bin = fix:pack(logon, [
%     %                 {reset_seq_num_flag, <<"Y">>},
%     %                 {encrypt_method,0},
%     %                 {sending_time,"1234"}], 2082, "CCLRA300", "OE101"),
%
%     % {ok,#heartbeat{sending_time=Sending,fields=Fields} = HB,_Rest,_C} = fix:decode( iolist_to_binary(Bin) ),
%
%     % [{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}] = Fields,
%     %
%     % ?assertEqual(<<"1234">>, Sending),
%     % ?assertEqual(<<"CCLRA300">>, Sender),
%     % ?assertEqual(<<"OE101">>, Target),
%     % ?assertEqual(2082, SeqNum),
%     error_logger:info_msg("Encoded message is ~n ~p ~n ~p ~n", fix:dump(Bin)),
%
%     ok.

quote_request_test() ->
  Bin = list_to_binary("8=FIX.4.49=29635=R34=71249=CLEAR52=20160115-20:17:10.62856=XPOMS131=31568_0146=255=PETR4T207=XBSP54=111=31569_038=10063=05497=205706=0.0121=400960=20160115-18:17:1044=16.19255=PETR4T207=XBSP54=211=31570_038=10063=05497=205706=0.0121=400860=20160115-18:17:1044=16.1921171=Y35004=010=046"),

  {ok,QR,_Rest,_C} = fix0:decode( Bin ),

  % error_logger:info_msg("Encoded message is ~n ~p ~n ~p ~n", fix:dump(Bin)),
  % error_logger:info_msg("Encoded message is ~n ~p ~n ~p ~n", fix:crack(QR)  ),
  fix0:crack(QR),

  ok.


position_maintenance_request_test() ->
  %com os parties
  Bin = list_to_binary("8=FIX.4.49=21935=AL34=449=CCLRA80152=20170126-18:10:41.25556=OE104C1=400455=BBDCM87E60=20170126-18:10:41453=3447=D452=36448=308447=D452=7448=DMA1447=D452=54702=1703=EX704=60000709=1710=28786_0712=1715=2017012610=102"),

  %sem os parties
  %Bin = list_to_binary("8=FIX.4.49=16235=AL34=249=CCLRA80152=20170126-22:47:08.14056=OE104C1=400455=ITUBM23E60=20170126-22:47:08453=0702=1703=EX704=500709=1710=28814_0712=1715=2017012610=204"),

  {ok,PR,_Rest,_C} = fix:decode( Bin ),

  error_logger:info_msg(" --- fez o decode. Deu nisto aqui ->>> ", PR),

  fix:crack(PR),

  ok.