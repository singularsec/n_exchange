-module(nexchange_trading_book_eventmgr).

-export([setup_handlers/0]).

-export([notify_fill/1]).

% API

setup_handlers() ->
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fixsession_dispatcher_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fillbook_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_book_logger_handler, []),
  ok.

notify_fill(Order) ->
  gen_event:sync_notify(nexchange_trading_book_eventmgr, {full_fill, Order}).
