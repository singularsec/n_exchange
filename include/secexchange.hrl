
-record(order, {oid,  % used for sorting only!
                symbol,
                id,
                
                qtd,
                qtd_filled,
                qtd_left,
                qtd_last,

                price,
                price_type,
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
                cl_ord_id
                }).

-record(orderbook, {sells=[], buys=[], lasttrade=0}).

-record(fillbook,  {sells=[], buys=[]}).

-record(execreport, {}).
