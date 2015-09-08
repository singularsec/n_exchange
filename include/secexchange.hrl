
-record(order, {oid, price, time, qtd, id, limit=undefined}).

-record(book,  {sells=[], buys=[]}).
