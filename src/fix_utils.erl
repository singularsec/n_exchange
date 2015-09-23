-module(fix_utils).

% -compile(export_all).
-export([extract_parties/1]).

-include("../include/secexchange.hrl").

% -record(execparty,       {id, source, role}).

extract_parties_item(Fields) when is_list(Fields) ->
  case lists:keyfind(party_id, 1, Fields) of
    false  -> [];
    {_,Id} ->
      {_,Sr} = lists:keyfind(party_id_source, 1, Fields),
      {_,Rl} = lists:keyfind(party_role, 1, Fields),

      L1 = lists:keydelete(party_id,   1, Fields),
      L2 = lists:keydelete(party_id_source, 1, L1),
      L3 = lists:keydelete(party_role, 1, L2),

      [ #execparty{id=Id, source=Sr, role=Rl} ] ++ extract_parties_item(L3)
  end.
  % keydelete(Key, N, TupleList1) -> TupleList2


extract_parties(Fields) when is_list(Fields) ->
%  {no_party_ids,3},
%  {party_id, <<"CCLRA301">>},
%  {party_id_source, propcode},
%  {party_role,36},
%  {party_id, <<"308">>},
%  {party_id_source, propcode},
%  {party_role,7},
%  {party_id, <<"DMA1">>},
%  {party_id_source,propcode},
%  {party_role,54}]}]
  NoParties = proplists:get_value(no_party_ids, Fields),
  case NoParties of
    Id when is_number(Id) ->
      NewList = lists:filter(fun ({K,_}) -> string:left(atom_to_list(K),6) == "party_" end, Fields),
      extract_parties_item(NewList);
    _ -> []
  end.





%
