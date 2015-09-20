-module(exec_report).

-compile(export_all).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").

build_accept(#order{} = Order) ->
  from_order(Order, accept, undefined).

build_partial_fill(#order{} = Order) ->
  from_order(Order, trade, undefined).

build_full_fill(#order{} = Order) ->
  from_order(Order, trade, undefined).

build_cancel(#order{} = Order, Reason) ->
  from_order(Order, canceled, Reason).

build_rejection(#order{} = Order, Reason) ->
  from_order(Order, rejected, Reason).


report_to_fix_bin(#execreport{from_sessionid=FromSessId,to_sessionid=DestSessId} = Report,
                  Seq) ->
  ReportPropList = record_to_proplist(Report),
  Body = to_fix44_body(ReportPropList),
  error_logger:info_msg("Body ~p ~n", [Body]),
  fix:pack(execution_report, Body, Seq, DestSessId, FromSessId).


from_order(#order{id=Id, from_sessionid=FromSessId, to_sessionid=DestSessId} = Order,
           ExecType, Reason) ->

  {Price,MatchingOrderId} = case Order#order.matches of
    [{P, _Qtd, Other}|_] ->
         {#execreportprice{avg=0, last=P, price=Order#order.price}, Other};
    _ -> {#execreportprice{}, undefined}
  end,

  Qtd = #execreportqtd{order_qtd= Order#order.qtd,
                       last= Order#order.qtd_last,
                       leaves= Order#order.qtd_left,
                       cum= Order#order.qtd_filled},

  Parties = [ #execparty{id="98",   source="D", role=36},
              #execparty{id="308",  source="D", role=7},
              #execparty{id="BVMF", source="D", role=54} ],

  ContraBrokers = [735],

  NewId = erlang:unique_integer([positive]),

  #execreport{order_id = Id,
              secondary_order_id = MatchingOrderId,
              exec_id = NewId,
              exec_type = ExecType,
              order_status = Order#order.order_status,
              order_type = Order#order.order_type,
              cl_ord_id = Order#order.cl_ord_id,
              % orig_cl_ord_id= ,
              account = Order#order.account,
              symbol = Order#order.symbol,
              side = Order#order.side,
              timeinforce = Order#order.timeinforce,
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

record_to_proplist(#execreportqtd{} = Rec) ->
  lists:zip(record_info(fields, execreportqtd), tl(tuple_to_list(Rec)));

record_to_proplist(#execreportprice{} = Rec) ->
  lists:zip(record_info(fields, execreportprice), tl(tuple_to_list(Rec))).

% -record(execreportqtd,   {order_qtd, last, leaves, cum}).
%   leaves_qty,
%   cum_qty,
%   last_qty,
%   underlying_last_qty,
to_fix44_body_qtd([]) -> [];
to_fix44_body_qtd([{last, undefined} | Rest])  -> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{last, Qtd} | Rest])        -> [{last_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{leaves, undefined} | Rest])-> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{leaves, Qtd} | Rest])      -> [{leaves_qty, Qtd}] ++ to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{cum, undefined} | Rest])   -> to_fix44_body_qtd(Rest);
to_fix44_body_qtd([{cum, Qtd} | Rest])         -> [{cum_qty, Qtd}] ++ to_fix44_body_qtd(Rest).

% -record(execreportprice, {avg, last, price}).
%   price,
%   stop_px,
%   last_px,
%   last_par_px,
%   avg_px,
to_fix44_body_price([]) -> [];
to_fix44_body_price([{price,undefined}|Rest]) -> to_fix44_body_price(Rest);
to_fix44_body_price([{price,Qtd}|Rest])       -> [{price, Qtd}] ++ to_fix44_body_price(Rest);
to_fix44_body_price([{avg,undefined} | Rest]) -> to_fix44_body_price(Rest);
to_fix44_body_price([{avg,Qtd} | Rest])       -> [{last_px, Qtd}] ++ to_fix44_body_price(Rest);
to_fix44_body_price([{last,undefined} | Rest])-> to_fix44_body_price(Rest);
to_fix44_body_price([{last,Qtd} | Rest])      -> [{last_px, Qtd}] ++ to_fix44_body_price(Rest).

% -record(execparty,       {id, source, role}).
to_fix44_body_party_item([]) -> [];
to_fix44_body_party_item([#execparty{id=Id, source=Source, role=Role} | Rest]) ->
  [{party_id, Id}, {party_id_source, Source}, {party_role, Role}] ++ to_fix44_body_contra_item(Rest).
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
  [{ord_type,V}] ++ to_fix44_body(Rest);

to_fix44_body([{sess_sub_id, V}|Rest]) ->
  [{ord_type,V}] ++ to_fix44_body(Rest);

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
  %  contra_brokers = [],
  % [{contra_brokers, aa}]
  Brokers ++ to_fix44_body(Rest);

% -record(execparty,       {id, source, role}).
to_fix44_body([{parties, PartyList}|Rest]) ->
  Fields = to_fix44_body_party(PartyList),
  %  fields = []
  % [{fields, Fields}] ++ to_fix44_body(Rest);
  Fields ++ to_fix44_body(Rest);

to_fix44_body([{Key, undefined} | Rest]) ->
  to_fix44_body(Rest);

to_fix44_body([{Key, Val} | Rest]) ->
  [{Key, Val}] ++ to_fix44_body(Rest).


% -record(execution_report, {
%   trade_origination_date,
%   exec_id,
%   ord_type,
%   price_type,
%   price,
%   stop_px,
%   last_px,
%   underlying_last_px,
%   last_par_px,
%   avg_px,
%   day_avg_px,
%   pegged_price,
%   discretion_price,
%   target_strategy,
%   target_strategy_parameters,
%   participation_rate,
%   target_strategy_performance,
%   currency,
%   compliance_id,
%   solicited_flag,
%   time_in_force,
%   effective_time,
%   expire_date,
%   expire_time,
%   exec_inst,
%   order_capacity,
%   order_restrictions,
%   cust_order_capacity,
%   leaves_qty,
%   cum_qty,
%   last_qty,
%   underlying_last_qty,
%   last_spot_rate,
%   last_forward_points,
%   last_mkt,
%   trading_session_id,
%   trading_session_sub_id,
%   time_bracket,
%   last_capacity,
%   day_order_qty,
%   day_cum_qty,
%   gt_booking_inst,
%   trade_date,
%   transact_time,
%   report_to_exch,
%   gross_trade_amt,
%   num_days_interest,
%   ex_date,
%   accrued_interest_rate,
%   accrued_interest_amt,
%   interest_at_maturity,
%   end_accrued_interest_amt,
%   start_cash,
%   end_cash,
%   traded_flat_switch,
%   basis_feature_date,
%   basis_feature_price,
%   concession,
%   total_takedown,
%   net_money,
%   settl_curr_amt,
%   settl_currency,
%   settl_curr_fx_rate,
%   settl_curr_fx_rate_calc,
%   handl_inst,
%   min_qty,
%   max_floor,
%   position_effect,
%   max_show,
%   booking_type,
%   text,
%   encoded_text,
%   settl_date2,
%   order_qty2,
%   last_forward_points2,
%   multi_leg_reporting_type,
%   cancellation_rights,
%   money_laundering_status,
%   regist_id,
%   designation,
%   trans_bkd_time,
%   exec_valuation_point,
%   exec_price_type,
%   exec_price_adjustment,
%   priority_indicator,
%   price_improvement,
%   last_liquidity_ind,
%   copy_msg_indicator,
%   protection_price,
%   unique_trade_id,
%   security_trading_statusb,
%   security_trading_statusc,
%   fixed_rate,
%   days_to_settlement,
%   memo,
%   appl_seq_num,
%   app_id,
%   order_category,
%   aggressor_indicator,
%   bts_final_tx_ord_status,
%   contra_brokers = [],
%   underlyings = [],
%   cont_amts = [],
%   legs = [],
%   misc_fees = [],
%   fields = []
%   tag10100,
%   tag10121,
%   tag10130,
%   tag10455,
%   tag10645,
%   tag10702,
%   tag10703,
% }).

% record_to_proplist(#foo{} = Rec) ->
%   lists:zip(record_info(fields, foo), tl(tuple_to_list(Rec))).


%
