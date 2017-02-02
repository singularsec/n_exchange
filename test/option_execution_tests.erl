-module(option_execution_tests).

-compile(export_all).

-include("../include/fix_session.hrl").

-include_lib("eunit/include/eunit.hrl").

handle_message_test() ->
  % uma mensagem enviada, com os Parties
  %Bin = list_to_binary("8=FIX.4.49=21935=AL34=449=CCLRA80152=20170126-18:10:41.25556=OE104C1=400455=BBDCM87E60=20170126-18:10:41453=3447=D452=36448=308447=D452=7448=DMA1447=D452=54702=1703=EX704=60000709=1710=28786_0712=1715=2017012610=102"),
   Bin = list_to_binary("8=FIX.4.49=22435=AL34=449=CCLRA80152=20170128-01:38:46.23056=OE104C1=400455=ITUBM23E60=20170128-01:38:46453=3448=98447=D452=36448=308447=D452=7448=BVMF447=D452=54702=1703=EX704=500709=1710=28861_0712=1715=2017012710=157"),

  {Messages, Rest} = fix_connection:decode_messages(Bin),

  InitialState = #state{our_seq=1, socket=undefined},

  fix_message_handler:handle_messages(Messages, Rest, InitialState),

  %error_logger:info_msg(" handle_message_test : Sucesso? [2] == ", NewState#state.our_seq),

  ok.