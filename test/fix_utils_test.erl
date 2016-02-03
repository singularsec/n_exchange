-module(fix_utils_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("../include/secexchange.hrl").


extract_parties_test() ->
  L = [{no_party_ids,3},
       {party_id, <<"CCLRA301">>}, {party_id_source, 1}, {party_role,36},
       {party_id, <<"308">>}, {party_id_source, 2}, {party_role,7},
       {party_id, <<"DMA1">>}, {party_id_source, 3}, {party_role,54}],

  Parties = fix_utils:extract_parties(L),
  Expected = [{execparty,<<"CCLRA301">>,1,36},
              {execparty,<<"308">>,2,7},
              {execparty,<<"DMA1">>,3,54}],

  % error_logger:info_msg("R ~p ~n", [Parties])

  ?_assertEqual(Expected, Parties).

extract_quote_request_legs_test() ->
  L = [{msg_seq_num,712},
       {sender_comp_id,<<"CLEAR">>},
       {target_comp_id,<<"XPOMS">>},
       {no_related_sym,2},
       {symbol,<<"PETR4T">>},
       {security_exchange,<<"XBSP">>},
       {order_qty,100},
       {5497,<<"20">>},
       {5706,<<"0.012">>},
       {symbol,<<"PETR4T1">>},
       {security_exchange,<<"XBSP">>},
       {order_qty,100},
       {5497,<<"20">>},
       {5706,<<"0.012">>},
       {1171,<<"Y">>},
       {35004,<<"0">>}],
  % -record(quote_request_leg, {symbol, secex, qty, daytosettlement,fixedrate}).
  Legs = fix_utils:extract_quote_request_legs(L),
  Expected = [{quote_request_leg,<<"PETR4T">>,1,36},
              {quote_request_leg,<<"PETR4T1">>,2,7}],

  error_logger:info_msg("R ~p ~n", [Legs]),

  ?_assertEqual(Expected, Legs).
