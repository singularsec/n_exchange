
-module(fix_parser44_tests).

-compile(export_all).

-include("../include/fix.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

-include_lib("eunit/include/eunit.hrl").

% sample_fix() ->
%   <<51,53,61,87,1,51,52,61,51,1,53,50,61,50,48,49,50,48,52,50,54,45,48,54,58,51,
%     51,58,48,51,46,53,49,54,1,53,53,61,85,82,75,65,1,50,54,50,61,52,50,1,50,54,
%     56,61,50,1,50,54,57,61,48,1,50,55,48,61,50,49,56,46,56,55,48,1,50,55,49,61,
%     50,48,1,50,54,57,61,49,1,50,55,48,61,50,49,57,46,48,51,48,1,50,55,49,61,49,
%     52,48,1>>.

% -spec pack(atom(), list(), non_neg_integer(), any(), any()) -> iolist().
% pack(MessageType, Body, SeqNum, Sender, Target)

hearbeat_encoding_test() ->
  Bin = fix:pack(heartbeat, [#heartbeat{}], 1, "Client", "Server"),

  C = fun (E) -> if E == 1 -> 124; true -> E end end,
  Bin2 = lists:map(C, binary_to_list(hd(Bin))),

  % error_logger:info_msg("Encoded message is ~p ~n ~s ~p ~n", [Bin, Bin2, 0]),

  Message = fix:decode( Bin ),

  ok.

% hearbeat_decoding_test() ->
%   % Subscribe = fix:pack(market_data_request, Request, 2, "TestClient", "TestServer")
%   % ?assertEqual(fix:sample_fix(), iolist_to_binary(fix:encode(fix_splitter:split(fix:sample_fix())))).
%   Bin = sample_fix(),
%   Message = fix:decode( Bin ),
%
%   error_logger:info_msg("Decoded message is ~s ~p ~n", [binary_to_list(Bin), Message]),
%
%   ok.
