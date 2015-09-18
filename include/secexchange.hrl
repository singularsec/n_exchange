
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
                matches=[]
                }).

-record(orderbook, {sells=[], buys=[]}).

-record(fillbook,  {sells=[], buys=[]}).

-record(execreport, {}).
