-module(quote_request_tests).

-compile(export_all).

-include("../include/fix_session.hrl").

-include_lib("eunit/include/eunit.hrl").

handle_message_test() ->
  % uma mensagem enviada, com os Parties
   Bin = list_to_binary("8=FIX.4.49=31135=R34=449=CLEAR_352=20170213-18:48:56.20356=XPOMS131=28775_0146=255=VALE1T207=XBSP54=111=28776_038=20063=05497=305706=0.0121=882338760=20170213-16:48:5644=5.6755=VALE1T207=XBSP54=211=28777_038=20063=05497=305706=0.0121=882337660=20170213-16:48:5644=5.671171=Y6004=CL_MESA35004=010=016"),

  {Messages, Rest} = fix_connection:decode_messages(Bin),

  InitialState = #state{our_seq=1, socket=undefined},

  fix_message_handler:handle_messages(Messages, Rest, InitialState),

  %error_logger:info_msg(" handle_message_test : Sucesso? [2] == ", NewState#state.our_seq),

  ok.