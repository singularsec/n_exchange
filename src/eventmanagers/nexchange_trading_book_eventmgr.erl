-module(nexchange_trading_book_eventmgr).

-export([setup_handlers/0, notify_sell/2]).

% API

setup_handlers() ->
  ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fixsession_dispatcher_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fillbook_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_book_logger_handler, []),
  ok.

notify_sell(SessionId, Symbol) ->
  % gen_event:notify(nexchange_trading_book_eventmgr, {session_authenticated, SessionId, Pid}).
  ok.
