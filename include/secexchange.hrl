
-record(order, {oid,
                price,
                time,
                qtd,
                id,
                ordertype,
                limit=undefined,
                timeinforce=day,
                expiration=none,
                account,
                sessionid,
                clOrdID,
                }).

-record(book,  {sells=[], buys=[], lasttrade=0}).

-record(execreport, {}).
