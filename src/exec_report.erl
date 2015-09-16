-module(exec_report).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").

build_accept() ->
  #execution_report{},
  ok.

build_reject() ->
  #execution_report{},
  ok.

% 20150716-14:51:11.317 : 8=FIX.4.4 | 9=293 |
% 35=8 | 34=1331 | 49=OE101 | 52=20150716-14:51:11.158 | 56=CCLRA300 |
% 1=307011 |       <--- Account
% 6=0 |            <--- AvgPx
% 11=32543_0 |     <--- ClOrdID
% 14=0 |           <--- CumQty
% 17=82249:74556 | <--- ExecID
% 37=8244561958 |  <--- OrderID
% 38=100 |         <--- OrderQty
% 39=0 |           <--- OrdStatus 0-New
% 40=2 |           <--- OrdType 2-Limit
% 44=11 |          <--- Price
% 54=1 |           <--- Side
% 55=PETR4 |       <--- Symbol
% 59=0 |           <--- TimeInForce
% 60=20150716-14:51:11.152 |  TransactTime
% 75=20150716 |    <--- TradeDate
% 150=0 |          <--- ExecType
% 151=100 |        <--- LeavesQty
% 198=8254423000 | <--- SecondaryOrderID
% 453=3 |          <--- NoPartyIDs
% 448=98 |         <--- PartyID
% 447=D |          <--- PartyIDSource
% 452=36 |         <--- PartyRole
% 448=308 |        <--- PartyID
% 447=D |          <--- PartyIDSource
% 452=7 |          <--- PartyRole
% 448=BVMF |       <--- PartyID
% 447=D |          <--- PartyIDSource
% 452=54 |         <--- PartyRole

build_exec_report(_ExecType, Seq, Target, Sender) ->
  % Rep = #execution_report{
  %   order_id=1,
  %   cl_ord_id=2
  % },

  Body = [],

  fix:pack(execution_report, Body, Seq, Target, Sender).
