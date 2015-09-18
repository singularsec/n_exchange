-module(exec_report).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

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


build_accept(#order{} = Order) ->
  #execreport{},
  ok.

build_partial_fill(#order{} = Order) ->
  #execreport{},
  ok.

build_full_fill(#order{} = Order) ->
  #execreport{},
  ok.

build_cancel(#order{} = Order, Reason) ->
  #execreport{},
  ok.

build_rejection(#order{} = Order, Reason) ->
  % 35=8 | 34=1605 | 49=OE101 | 52=20150716-16:21:39.433 | 56=CCLRA300 | 1=307003 | 6=0 |
  % 11=32547_1 | 14=200 | 17=80247:2138:1 | 31=16.2 | 32=200 | 37=8075190607 | 38=200 | 39=2 |
  % 40=2 | 41=32547_0 | 44=16.2 | 54=2 | 55=VALE5 | 59=0 | 60=20150716-16:21:39.437 |
  % 75=20150716 | 150=F | 151=0 | 198=8080458418 | 336=1 | 382=1 | 375=735 |
  % 453=3 | 448=CCLRA300 | 447=D |
  % 452=36 | 448=308 | 447=D |
  % 452=7 | 448=DMA1 | 447=D |
  % 452=54 | 581=39 | 625=17 | 1057=N | 6032=170

  % #execution_report{},
  ok.

% build_exec_report(ExecType, Body, Seq, Target, Sender) ->
%   % Rep = #execution_report{
%   %   order_id=1,
%   %   cl_ord_id=2
%   % },
%   % Body = [],
%   % fix:pack(execution_report, Body, Seq, Target, Sender).
%   ok.

% body_from_order(#order{} = Order) ->
%   [].
% body_from_new_order_single(#new_order_single{} = Order) ->
%   [].





%
