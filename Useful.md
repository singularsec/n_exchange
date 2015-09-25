
Shell
==

-- loads all record defs from this module

rr(nexchange_trading_book).

-- creates or access an book:

Petro = nexchange_bookregistry:get_book("PETR4").



-- Dumps buys/sells open entries in the book (not a filled book)

nexchange_trading_book:dump_book(Petro).


-- Adds a sell Order

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",
                cl_ord_id="cl1",
                qtd=100,
                qtd_left=100,
                price=10,
                side=sell,
                order_type=limit, timeinforce=day,
                account="acc1", from_sessionid="CCLR", to_sessionid="SOME"}).


-- Adds a buy Order

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",
                cl_ord_id="cl2",
                qtd=100,
                qtd_left=100,
                price=10,
                side=buy,
                order_type=limit,timeinforce=day,account="acc2",
                from_sessionid="CCLR",to_sessionid="SOME"}).
