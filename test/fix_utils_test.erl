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
