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

get_and_remove(Atom, Lst) when is_atom(Atom) ->
  case lists:keyfind(Atom, 1, Lst) of
    false -> {undefined, Lst};
    {_, Value} ->
      NewLst = lists:keydelete(Atom, 1, Lst),
      {Value, NewLst}
  end.

extract_quote_request_legs_item(Fields) when is_list(Fields) ->
  case lists:keyfind(symbol, 1, Fields) of
    false  -> [];
    {_,Symbol} ->
      {SecE,NewList1}  = get_and_remove(security_exchange, Fields),
      {Side,NewList2}  = get_and_remove(side, NewList1),
      {ClOr,NewList3}  = get_and_remove(cl_ord_id, NewList2),
      {Qtdy,NewList4}  = get_and_remove(order_qty, NewList3),
      {DayS,NewList5}  = get_and_remove(days_to_settlement, NewList4),
      {SetT,NewList6}  = get_and_remove(settl_type, NewList5),
      {FixR,NewList7}  = get_and_remove(fixed_rate, NewList6),
      {Acct,NewList8}  = get_and_remove(account, NewList7),
      {Tran,NewList9}  = get_and_remove(transact_time, NewList8),
      {Px,  NewList10} = get_and_remove(price, NewList9),
      FinalList = lists:keydelete(symbol, 1, NewList10),
      
      [#quote_request_leg{symbol=Symbol, account=Acct, side=Side, cl_ord_id=ClOr,
                          secex=SecE, order_qty=Qtdy, settl_type=SetT,
                          days_to_settlement=DayS, fixed_rate=FixR,
                          transact_time=Tran, price=Px
                          }]
      ++ extract_quote_request_legs_item(FinalList)
  end.

extract_quote_request_legs(Fields) when is_list(Fields) ->
% {no_related_sym,2},
% {symbol,<<"PETR4T">>},
% {security_exchange,<<"XBSP">>},
% {side,buy},
% {cl_ord_id,<<"31569_0">>},
% {order_qty,100},
% {settl_type,regular},
% {days_to_settlement,<<"20">>},
% {fixed_rate,<<"0.012">>},
% {account,<<"4009">>},
% {transact_time,<<"20160115-18:17:10">>},
% {price,16.192},
  NoLegs = proplists:get_value(no_related_sym, Fields),
  case NoLegs of
    Id when is_number(Id) ->
      NewList = lists:filter(fun ({K,_}) -> extract_quote_request_legs_atoms(K) end, Fields),
      extract_quote_request_legs_item(NewList);
    _ -> []
  end.

extract_quote_request_legs_atoms(symbol) -> true;
extract_quote_request_legs_atoms(security_exchange) -> true;
extract_quote_request_legs_atoms(side) -> true;
extract_quote_request_legs_atoms(cl_ord_id) -> true;
extract_quote_request_legs_atoms(settl_type) -> true;
extract_quote_request_legs_atoms(order_qty) -> true;
extract_quote_request_legs_atoms(days_to_settlement) -> true;
extract_quote_request_legs_atoms(fixed_rate) -> true;
extract_quote_request_legs_atoms(account) -> true;
extract_quote_request_legs_atoms(transact_time) -> true;
extract_quote_request_legs_atoms(price) -> true;
extract_quote_request_legs_atoms(_) -> false.
