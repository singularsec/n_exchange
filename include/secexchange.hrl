
-record(order, {symbol,
                cl_ord_id,
                qtd,
                qtd_filled,
                qtd_left,
                qtd_last,
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

-record(orderbook, {sells=[], buys=[]}).

-record(fillbook,  {sells=[], buys=[]}).

-record(execreportqtd,   {order_qtd, last, leaves, cum}).
-record(execreportprice, {avg, last, price}).
-record(execparty,       {id, source, role}).

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
                     price,  % execreportprice#
                     contrabrokers=[],
                     parties=[],
                     text,   % for error messages
                     sess_id=1,
                     sess_sub_id=17,
                     from_sessionid,
                     to_sessionid
                    }).
