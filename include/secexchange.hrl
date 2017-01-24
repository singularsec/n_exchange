
-record(order, {symbol,
                cl_ord_id,
                orig_cl_ord_id=undefined,
                order_qty=0, % original qtd
                qtd_filled=0,
                qtd_left=0,
                qtd_last=0,
                price,
                price_type,
                order_status,
                time,
                order_type,
                side,
                stop_price,
                timeinforce=day,
                expirationd=none,
                expirationt=none,
                account,
                from_sessionid,
                to_sessionid,
                id,
                oid,  % used for sorting only!
                matches=[],
                parties=[]
                }).

-record(orderbook, {sells=[], buys=[], lasttrade=0, symbol}).

-record(fillbook,  {sells=[], buys=[]}).

-record(execreportqtd,   {order_qty, last, leaves, cum}).
-record(execreportprice, {avg, last, price}).
-record(execparty,       {id, source, role}).

-record(quote_request_leg, {symbol,
                            secex, side,
                            cl_ord_id,
                            order_qty,
                            days_to_settlement,
                            settl_type, account, transact_time,
                            fixed_rate, price }).

-record(execreport, {order_id,
                     secondary_order_id,
                     exec_id,
                     exec_type,
                     order_status,
                     order_type,
                     cl_ord_id,
                     orig_cl_ord_id,
                     account,
                     symbol,
                     side,
                     time_in_force,
                     transact_time, trade_date,
                     qtd,    % execreportqtd#
                     % order_qty = undefined,
                     price,  % execreportprice#
                     contrabrokers=[],
                     parties=[],
                     text,   % for error messages
                     sess_id=1,
                     sess_sub_id=17,
                     ord_rej_reason, security_exchange, unique_trade_id,
                     from_sessionid,
                     to_sessionid
                    }).

-record(position_maintenance_report,
  {
    sending_time,
    pos_maint_rpt_id,
    pos_trans_type,
    pos_req_id,
    pos_maint_action,
    orig_pos_req_ref_id,
    pos_maint_status,
    pos_maint_result,
    clearing_business_date,
    no_party_ids,
    party_id_source,
    party_id,
    party_role,
    trade_id,
    account,
    account_type,
    symbol,
    transact_time,
    no_positions,
    pos_type,
    long_qty,
    threshold_amount,
    from_sessionid,
    to_sessionid
}).

-record(cancelreject, {order_id,
                     secondary_order_id,
                     exec_id,
                     exec_type,
                     order_status,
                     order_type,
                     cl_ord_id,
                     orig_cl_ord_id,
                     account,
                     symbol,
                     side,
                     time_in_force,
                     transact_time, trade_date,
                     qtd,    % execreportqtd#
                     price,  % execreportprice#
                     contrabrokers=[],
                     parties=[],
                     text,   % for error messages
                     sess_id=1,
                     sess_sub_id=17,
                     ord_rej_reason, security_exchange, unique_trade_id,
                     from_sessionid,
                     to_sessionid
                    }).

-record(tradeinfo, {symbol,
                    refid,
                    price,
                    qtd,
                    buyer, seller
                    }).

-record(order_cancel, {order_id, 
                       cl_ord_id, orig_cl_ord_id,
                       account,
                       from_sessionid,
                       to_sessionid,
                       side,
                       symbol,
                       parties=[]
                      }).

-record(order_modify, {order_id, 
                       cl_ord_id, orig_cl_ord_id,
                       order_qty,    
                       price,
                       price_type,
                       account,
                       from_sessionid,
                       to_sessionid,
                       side,
                       symbol,
                       parties=[]
                      }).
