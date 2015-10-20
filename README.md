# n_exchange

Portions from https://github.com/maxlapshin/fix



fix sessions
==

fix protocol
==

driver
==


http://stackoverflow.com/questions/13112062/which-are-the-order-matching-algorithms-most-commonly-used-by-electronic-financi#

book and matching engine
==

Before the matching process starts, the entered buy and sell orders are arranged or prioritised according to price-time priority on both sides of the order book. Specifically, this means for the

buyer: market buy orders are executed first, followed by limit orders (from the highest limit to the lowest).
Seller: market sell orders are executed first, followed by limit orders (from the lowest limit to the highest).
Next, orders placed at the same price are executed according to the time of entry (time priority).

C++ engine
==

https://github.com/objectcomputing/liquibook/tree/master/src/book


Order types
==

Unlimited:	If you place an unlimited order, it is always executed, at least in the case of tradeable shares. You will receive the shares in question at the lowest (= best) price currently offered in the order book. If the volume you have entered is larger, the system matches with the second-best price until the entire order is executed. Sometimes there are no sell offers for little-traded securities. In this case, the unlimited order remains in the order book until it can be executed.

Limit:	Limit orders differ from unlimited orders in that they are subject to a price limit. When placing a normal order with a limit, you determine a specific price at which you are willing to buy the desired number of shares. The system treats all offers present in the order book that are below your limit as unlimited orders. However, no execution takes place at prices above your limit. The disadvantage of this type of order is that it will not be matched if your limit is too low.



UI Manager:
-- shows all sessions, and their realtime books
   - show message history per connection

Driver creates:
-- Create private fix session
   fix session has books

-- sets up instruments



SecExchange _ Application:
  supervisor
    driver
      tcp_listener
        driver_session
          sets up:

          tcp_listener for:
            fix_session (ports)

              admin level: logon/messages
              config -> spawn -> hearbeat func
              symbol_table -> book

              (low) book changes publish events -> future to receiver?


Book FIFO (more resources at https://sites.google.com/site/rajeevranjansingh/downloads)
  Sort sells by time/price ASC
  Sort Buys  by time/price DESC

  http://web.archive.org/web/20120626161034/http://www.cmegroup.com/confluence/display/EPICSANDBOX/Match+Algorithms

  FIFO sample

  http://web.archive.org/web/20120712093326/http://www.cmegroup.com/confluence/display/EPICSANDBOX/Match+Algorithm+-+FIFO



order types we support

https://github.com/clearctvm/Clear.Foundation.Trade/blob/master/src/Clear.Foundation.Trade.Messages/Enums/ExchangeOrderType.cs

Market = '1',
		Limit = '2',
		Stop = '3',
		StopLimit = '4',
		MarketWithLeftover = 'k',
        Cross = '9',
		OnClose = 'A',
		SimultaneousStop = 'X',
		StopMarket = 'Y',
		OneTriggerOther = 'W',
		OneTriggerOtherScalper = 'S'

Fix dictionary http://www.onixs.biz/fix-dictionary/4.3/msgs_by_msg_type.html



;
