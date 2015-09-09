
-record(order, {oid, price, time, qtd, id, limit=undefined, timeinforce=day, expiration=none}).

-record(book,  {sells=[], buys=[], lasttrade=0}).

-record(execreport, {}).
