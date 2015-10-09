-module(nexchange_trading_book_eventmgr).

-compile(export_all).


% API

setup_handlers() ->
  ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fixsession_dispatcher_handler, []),
  ok = gen_event:add_handler(nexchange_trading_book_eventmgr, pub_to_receiver_handler, []),

  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fillbook_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_book_logger_handler, []),
  ok.

notify_trade(TradeInfo) ->
    error_logger:info_msg("notify_trade ~n ~p ~n", [TradeInfo]),
    gen_event:notify(nexchange_trading_book_eventmgr, {new_trade, TradeInfo}).    

-spec notify_accept('order') -> ok.
notify_accept(Order) ->
  error_logger:info_msg("notify_accept ~n ~p ~n", [Order]),
  gen_event:notify(nexchange_trading_book_eventmgr, {accept, Order}).


-spec notify_fill('order') -> ok.
notify_fill(Order) ->
  error_logger:info_msg("notify_fill ~n ~p ~n", [Order]),
  gen_event:notify(nexchange_trading_book_eventmgr, {full_fill, Order}).


-spec notify_partial_fill('order') -> ok.
notify_partial_fill(Order) ->
  error_logger:info_msg("notify_partial_fill ~n ~p ~n", [Order]),
  gen_event:notify(nexchange_trading_book_eventmgr, {partial_fill, Order}).


-spec notify_rejection('order', any()) -> ok.
notify_rejection(Order, Reason) ->
  error_logger:info_msg("notify_rejection ~n ~p ~n ~p ~n", [Order, Reason]),
  gen_event:notify(nexchange_trading_book_eventmgr, {rejection, {Order, Reason}}).


-spec notify_cancel('order', any()) -> ok.
notify_cancel(Order, Reason) ->
  error_logger:info_msg("notify_cancel ~n ~p ~n ~p ~n", [Order, Reason]),
  gen_event:notify(nexchange_trading_book_eventmgr, {cancel, {Order, Reason}}).



%
