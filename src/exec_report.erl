-module(exec_report).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44_xp.hrl").
-include("../include/business44_xp.hrl").
-include("../include/secexchange.hrl").

build_accept(#order{} = Order) ->
  from_order(Order, new, undefined).

build_partial_fill(#order{} = Order) ->
  from_order(Order, trade, undefined).

build_full_fill(#order{} = Order) ->
  from_order(Order, trade, undefined).

build_cancel(#order{} = Order, Reason) ->
  from_order(Order, canceled, Reason);

build_cancel(#order_cancel_request{} = Order, Reason) ->
  from_order(Order, canceled, Reason).

build_rejection(#order{} = Order, Reason) ->
  from_order(Order, rejected, Reason).

build_replace(#order{} = Order) ->
  from_order(Order, replace, undefined).

% this is not exactly an execution report
build_cancel_reject(#order_cancel_request{} = Order, Reason) ->
  cancel_reject_from_order(Order, Reason).

build_accept_for_quote_request_leg(#quote_request{} = QR, #quote_request_leg{} = Leg, QuoteId) ->
  NewId = erlang:unique_integer([positive]),
  Qtd = #execreportqtd{order_qty= Leg#quote_request_leg.order_qty,
                       last= 0,
                       leaves= Leg#quote_request_leg.order_qty,
                       cum= 0},
  Rate = list_to_float(binary_to_list(Leg#quote_request_leg.fixed_rate)),
  Px = ((Leg#quote_request_leg.price * 10000.0) * Rate) + (Leg#quote_request_leg.price * 10000.0),
  Price = #execreportprice{avg=Px, last=Px, price=Px},
  Fields = QR#quote_request.fields,
  % FromSessId = proplists:get_value(target_comp_id, Fields),
  % DestSessId = proplists:get_value(sender_comp_id, Fields),
  Id = "000000" ++ QuoteId,
  #execreport{order_id = list_to_binary(Id),
              secondary_order_id = list_to_binary("800_" ++ Id),
              exec_id = NewId,
              exec_type = new,
              order_status = new,
              order_type = limit,
              cl_ord_id = Leg#quote_request_leg.cl_ord_id,
              account = Leg#quote_request_leg.account,
              symbol = Leg#quote_request_leg.symbol,
              side = Leg#quote_request_leg.side,
              time_in_force = fill_or_kill,
              qtd = Qtd,
              price = Price,
              ord_rej_reason = 99, % needs constant?
              security_exchange = <<"XBSP">>,
              % from_sessionid = DestSessId,
              % to_sessionid = FromSessId
              to_sessionid   = binary_to_list( proplists:get_value(target_comp_id, Fields) ),
              from_sessionid = binary_to_list( proplists:get_value(sender_comp_id, Fields) )
              }.

build_filled_for_quote_request_leg(#quote_request{} = QR, #quote_request_leg{} = Leg, QuoteId) ->

  NewId = erlang:unique_integer([positive]),
  Qtd = #execreportqtd{order_qty= Leg#quote_request_leg.order_qty,
                       last= Leg#quote_request_leg.order_qty,
                       leaves= 0,
                       cum= Leg#quote_request_leg.order_qty},
  Rate = list_to_float(binary_to_list(Leg#quote_request_leg.fixed_rate)),
  Px = ((Leg#quote_request_leg.price * 10000.0) * Rate) + (Leg#quote_request_leg.price * 10000.0),
  Price = #execreportprice{avg=Px, last=Px, price=Px},
  Fields = QR#quote_request.fields,

  % FromSessId = proplists:get_value(target_comp_id, Fields),
  % DestSessId = proplists:get_value(sender_comp_id, Fields),
  Id = "000000" ++ QuoteId,
  #execreport{order_id = Id,
              secondary_order_id = "800_" ++ Id,
              exec_id = NewId,
              exec_type = trade,
              order_status = filled,
              order_type = limit,
              cl_ord_id = Leg#quote_request_leg.cl_ord_id,
              account = Leg#quote_request_leg.account,
              symbol = Leg#quote_request_leg.symbol,
              side = Leg#quote_request_leg.side,
              time_in_force = fill_or_kill,
              qtd = Qtd,
              price = Price,
              unique_trade_id = NewId,
              %from_sessionid = DestSessId,
              %to_sessionid = FromSessId
              to_sessionid   = binary_to_list( proplists:get_value(target_comp_id, Fields) ),
              from_sessionid = binary_to_list( proplists:get_value(sender_comp_id, Fields) )
              }.

report_to_fix_bin(#execreport{from_sessionid=FromSessId,to_sessionid=DestSessId} = Report,
                  Seq) ->
  ReportPropList = record_to_proplist(Report),
  Body = to_fix44_body(ReportPropList),
  % error_logger:info_msg("Body ~p ~n", [Body]),
  fix0:pack(execution_report, Body, Seq, DestSessId, FromSessId);


report_to_fix_bin(#cancelreject{from_sessionid=FromSessId,to_sessionid=DestSessId} = Report,
                  Seq) ->
  ReportPropList = record_to_proplist(Report),
  Body = to_fix44_body(ReportPropList),
  % error_logger:info_msg("Body ~p ~n", [Body]),
  fix0:pack(order_cancel_reject, Body, Seq, DestSessId, FromSessId).


cancel_reject_from_order(#order_cancel_request{} = Order, Reason) ->

  NewId = erlang:unique_integer([positive]),

  Fields     = Order#order_cancel_request.fields,
  FromSessId = proplists:get_value(target_comp_id, Fields),
  DestSessId = proplists:get_value(sender_comp_id, Fields),
  Symbol     = proplists:get_value(symbol, Fields),

  Parties = fix_utils:extract_parties(Fields),

  error_logger:info_msg("exec report rejected cancel ~p ~n", [Order#order_cancel_request.order_id]),

  #cancelreject{order_id = Order#order_cancel_request.order_id,
               account = Order#order_cancel_request.account,
               order_status = rejected,
               order_type = limit,
               time_in_force = day,
               cl_ord_id = Order#order_cancel_request.cl_ord_id,
               orig_cl_ord_id= Order#order_cancel_request.orig_cl_ord_id,
               symbol = Symbol,
               side = Order#order_cancel_request.side,
               % transact_time= % 60=20150716-14:51:11.152 |  TransactTime
               % trade_date= ,% 75=20150716 |    <--- TradeDate
               contrabrokers = [735],
               parties = Parties,
               text = Reason,
               from_sessionid = FromSessId,
               to_sessionid = DestSessId
               }.


from_order(#order_cancel_request{} = Order, ExecType, _Reason) ->

  NewId = erlang:unique_integer([positive]),

  Fields     = Order#order_cancel_request.fields,
  FromSessId = proplists:get_value(target_comp_id, Fields),
  DestSessId = proplists:get_value(sender_comp_id, Fields),
  Symbol     = proplists:get_value(symbol, Fields),

  Parties = fix_utils:extract_parties(Fields),

  error_logger:info_msg("exec report rejected cancel ~p ~n", [ExecType, Order#order_cancel_request.order_id]),

  #execreport{order_id = Order#order_cancel_request.order_id,
             exec_id = NewId,
             exec_type = ExecType,
             account = Order#order_cancel_request.account,
             order_status = canceled,
             order_type = limit,
             time_in_force = day,
             cl_ord_id = Order#order_cancel_request.cl_ord_id,
             orig_cl_ord_id= Order#order_cancel_request.orig_cl_ord_id,
             symbol = Symbol,
             side = Order#order_cancel_request.side,
             % transact_time= % 60=20150716-14:51:11.152 |  TransactTime
             % trade_date= ,% 75=20150716 |    <--- TradeDate
             qtd = #execreportqtd{order_qty= 0,
                                  last= 0,
                                  leaves= 10,
                                  cum= 0},
             price = #execreportprice{avg=0},
             contrabrokers = [735],
             parties = Parties,
             % text = undefined,
             from_sessionid = FromSessId,
             to_sessionid = DestSessId
             };


from_order(#order{id=Id, from_sessionid=FromSessId, to_sessionid=DestSessId} = Order, ExecType, Reason) ->

  {Price,MatchingOrderId} = case Order#order.matches of
    [{P, _Qtd, Other}|_] -> {#execreportprice{avg=0, last=P, price=Order#order.price}, Other};
    _                    -> {#execreportprice{avg=0}, undefined}
  end,

  Qtd = #execreportqtd{order_qty= Order#order.order_qty,
                       last= Order#order.qtd_last,
                       leaves= Order#order.qtd_left,
                       cum= Order#order.qtd_filled},

  % Parties = [ #execparty{id="98",   source="D", role=36},
  %             #execparty{id="308",  source="D", role=7},
  %             % #execparty{id="BVMF", source="D", role=54} ],
  %             #execparty{id="BVMF", source="D", role=4} ],
  Parties = Order#order.parties,

  ContraBrokers = [735],

  NewId = erlang:unique_integer([positive]),

  #execreport{order_id = Id,
              secondary_order_id = MatchingOrderId,
              exec_id = NewId,
              exec_type = ExecType,
              order_status = Order#order.order_status,
              order_type = Order#order.order_type,
              cl_ord_id = Order#order.cl_ord_id,
              orig_cl_ord_id = Order#order.orig_cl_ord_id,
              account = Order#order.account,
              symbol = Order#order.symbol,
              side = Order#order.side,
              time_in_force = Order#order.timeinforce,
              % transact_time= % 60=20150716-14:51:11.152 |  TransactTime
              % trade_date= ,% 75=20150716 |    <--- TradeDate
              qtd = Qtd,
              price = Price,
              contrabrokers = ContraBrokers,
              parties = Parties,
              text = Reason,
              from_sessionid = FromSessId,
              to_sessionid = DestSessId
              }.


record_to_proplist(undefined) -> [];

record_to_proplist(#execreport{} = Rec) ->
  lists:zip(record_info(fields, execreport), tl(tuple_to_list(Rec)));

record_to_proplist(#cancelreject{} = Rec) ->
  lists:zip(record_info(fields, cancelreject), tl(tuple_to_list(Rec)));

record_to_proplist(#execreportqtd{} = Rec) ->
  lists:zip(record_info(fields, execreportqtd), tl(tuple_to_list(Rec)));

record_to_proplist(#execreportprice{} = Rec) ->
  lists:zip(record_info(fields, execreportprice), tl(tuple_to_list(Rec))).

% -record(execreportqtd,   {order_qty, last, leaves, cum}).
%   leaves_qty,
%   cum_qty,
%   last_qty,
%   underlying_last_qty,
to_fix44_body_qtd([]) -> [];
to_fix44_body_qtd([{order_qty, undefined} | Rest])  -> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{order_qty, Qtd} | Rest])        -> [{order_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{last, undefined} | Rest])  -> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{last, Qtd} | Rest])        -> [{last_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{leaves, undefined} | Rest])-> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{leaves, Qtd} | Rest])      -> [{leaves_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{cum, undefined} | Rest])   -> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{cum, Qtd} | Rest])         -> [{cum_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{_, _} | Rest])             -> to_fix44_body_qtd(Rest).

% -record(execreportprice, {avg, last, price}).
%   price,
%   stop_px,
%   last_px,
%   last_par_px,
%   avg_px,
to_fix44_body_price([]) -> [];
to_fix44_body_price([{price,undefined}|Rest]) -> to_fix44_body_price(Rest);
to_fix44_body_price([{price, P}|Rest])        -> [{price, P / 10000.0}] ++ to_fix44_body_price(Rest);
to_fix44_body_price([{avg,undefined} | Rest]) -> to_fix44_body_price(Rest);
to_fix44_body_price([{avg, P} | Rest])        -> [{avg_px, P / 10000.0}] ++ to_fix44_body_price(Rest);
to_fix44_body_price([{last,undefined} | Rest])-> to_fix44_body_price(Rest);
to_fix44_body_price([{last, P} | Rest])       -> [{last_px, P / 10000.0}] ++ to_fix44_body_price(Rest).

% -record(execparty,       {id, source, role}).
to_fix44_body_party_item([]) -> [];
to_fix44_body_party_item([#execparty{id=Id, source=Source, role=Role} | Rest]) ->
  [{party_id, Id}, {party_id_source, Source}, {party_role, Role}] ++ to_fix44_body_party_item(Rest).
to_fix44_body_party(Parties) ->
  HowMany = length(Parties),
  [{no_party_ids, HowMany}] ++ to_fix44_body_party_item(Parties).


to_fix44_body_contra_item([]) -> [];
to_fix44_body_contra_item([BrokerId| Rest]) ->
  [{contra_broker, BrokerId}] ++ to_fix44_body_contra_item(Rest).
to_fix44_body_contra(Brokers) ->
  HowMany = length(Brokers),
  [{no_contra_brokers, HowMany}] ++ to_fix44_body_contra_item(Brokers).



to_fix44_body([]) -> [];

to_fix44_body([{order_status, undefined}|Rest]) -> to_fix44_body(Rest);

to_fix44_body([{order_status, V}|Rest]) ->
  [{ord_status,V}] ++ to_fix44_body(Rest);


to_fix44_body([{order_type, undefined}|Rest]) -> to_fix44_body(Rest);

to_fix44_body([{order_type, V}|Rest]) ->
  [{ord_type,V}] ++ to_fix44_body(Rest);

to_fix44_body([{sess_id, V}|Rest]) ->
  [{trading_session_id,V}] ++ to_fix44_body(Rest);

to_fix44_body([{sess_sub_id, V}|Rest]) ->
  [{trading_session_sub_id,V}] ++ to_fix44_body(Rest);

to_fix44_body([{from_sessionid, _}|Rest]) -> to_fix44_body(Rest);
to_fix44_body([{to_sessionid, _}|Rest]) -> to_fix44_body(Rest);

to_fix44_body([{qtd, V}|Rest]) ->
  List = record_to_proplist(V),
  Body = to_fix44_body_qtd(List),
  Body ++ to_fix44_body(Rest);

to_fix44_body([{price, V}|Rest]) ->
  List = record_to_proplist(V),
  Body = to_fix44_body_price(List),
  Body ++ to_fix44_body(Rest);

to_fix44_body([{contrabrokers, V}|Rest]) ->
  Brokers = to_fix44_body_contra(V),
  Brokers ++ to_fix44_body(Rest);

% -record(execparty,       {id, source, role}).
to_fix44_body([{parties, PartyList}|Rest]) ->
  Fields = to_fix44_body_party(PartyList),
  Fields ++ to_fix44_body(Rest);

to_fix44_body([{Key, undefined} | Rest]) ->
  to_fix44_body(Rest);

to_fix44_body([{Key, Val} | Rest]) ->
  [{Key, Val}] ++ to_fix44_body(Rest).



%
