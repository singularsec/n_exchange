-module(fix_utils).

-compile(export_all).
% -export([extract_parties/1]).

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



extract_quote_request_legs_item(Fields) when is_list(Fields) ->
  case lists:keyfind(symbol, 1, Fields) of
    false  -> [];
    {_,Symbol} ->
      {_,SecE} = lists:keyfind(security_exchange, 1, Fields),
      {_,Qtdy} = lists:keyfind(order_qty, 1, Fields),
      {_,DayS} = lists:keyfind(5497, 1, Fields),
      {_,FixR} = lists:keyfind(5706, 1, Fields),

      L1 = lists:keydelete(symbol, 1, Fields),
      L2 = lists:keydelete(security_exchange, 1, L1),
      L3 = lists:keydelete(order_qty, 1, L2),
      L4 = lists:keydelete(5497, 1, L3),
      L5 = lists:keydelete(5706, 1, L4),

      % -record(quote_request_leg, {symbol, secex, qty, daytosettlement, fixedrate}).
      [#quote_request_leg{symbol=Symbol, secex=SecE, qty=Qtdy, daytosettlement=DayS, fixedrate=FixR}]
      ++ extract_quote_request_legs_item(L5)
  end.
  % keydelete(Key, N, TupleList1) -> TupleList2

extract_quote_request_legs(Fields) when is_list(Fields) ->
% {no_related_sym,2},
% {symbol,<<"PETR4T">>},
% {security_exchange,<<"XBSP">>},
% {order_qty,100},
% {5497,<<"20">>},
% {5706,<<"0.012">>},
% {symbol,<<"PETR4T">>},
% {security_exchange,<<"XBSP">>},
% {order_qty,100},
% {5497,<<"20">>},
% {5706,<<"0.012">>}]
  NoLegs = proplists:get_value(no_related_sym, Fields),
  case NoLegs of
    Id when is_number(Id) ->
      NewList = lists:filter(fun ({K,_}) -> extract_quote_request_legs_atoms(K) end, Fields),
      extract_quote_request_legs_item(NewList);
    _ -> []
  end.

extract_quote_request_legs_atoms(symbol) -> true;
extract_quote_request_legs_atoms(security_exchange) -> true;
extract_quote_request_legs_atoms(order_qty) -> true;
extract_quote_request_legs_atoms(5497) -> true;
extract_quote_request_legs_atoms(5706) -> true;
extract_quote_request_legs_atoms(_) -> false.




%
