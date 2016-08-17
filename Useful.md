
Shell
==

-- loads all record defs from this module

rr(nexchange_trading_book).

-- creates or access an book:

Petro = nexchange_bookregistry:get_book("PETR4"). 
Vale = nexchange_bookregistry:get_book("VALE5"). 
Bvmf = nexchange_bookregistry:get_book("BVMF3"). 



-- Dumps buys/sells open entries in the book (not a filled book)

nexchange_trading_book:dump_book(Petro).

nexchange_trading_book:qa_fillbook(Petro). 

nexchange_trading_book:qa_fillbook(Vale). 

nexchange_trading_book:qa_fillbook(Bvmf). 




-- Adds a sell Order

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",cl_ord_id="cl1",
                qtd=100,qtd_left=100,price=10,side=sell,order_type=limit, timeinforce=day, account="acc1", from_sessionid="CCLR", to_sessionid="SOME"}).


-- Adds a buy Order

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",
                cl_ord_id="cl2",qtd=100, qtd_left=100, price=10, side=buy, order_type=limit,timeinforce=day,account="acc2", from_sessionid="CCLR",to_sessionid="SOME"}).



-- Cancel order 

nexchange_trading_book:try_cancel(Petro, #order_cancel{symbol="PETR4", cl_ord_id="cl2", side=buy, account="acc2", from_sessionid="CCLR",to_sessionid="SOME"}).


-- Replace order


-- All

rr(nexchange_trading_book). 
Petro = nexchange_bookregistry:get_book("PETR4").
nexchange_trading_book:dump_book(Petro).

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",cl_ord_id="cl1",
                qtd=100,qtd_left=100,price=10,side=sell,order_type=limit, timeinforce=day, account="acc1", from_sessionid="CCLR", to_sessionid="SOME"}).

nexchange_trading_book:send_new_order_single(Petro, #order{symbol="PETR4",cl_ord_id="cl2",
                qtd=100, qtd_left=100, price=10, side=buy, order_type=limit,timeinforce=day,account="acc2", from_sessionid="CCLR",to_sessionid="SOME"}).











Bbdc = nexchange_bookregistry:get_book("BBDC4").
nexchange_trading_book:dump_book(Bbdc).




.
