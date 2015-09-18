-module(nexchange_trading_book_eventmgr).

% -export([setup_handlers/0]).
% -export([notify_fill/1]).
-compile(export_all).

% -ifdef(EUNIT).
% -else.
% -endif.

% API

setup_handlers() ->
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fixsession_dispatcher_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_fillbook_handler, []),
  % ok = gen_event:add_handler(nexchange_trading_book_eventmgr, nexchange_book_logger_handler, []),
  ok.

-spec notify_accept('order') -> ok.
notify_accept(Order) ->
  gen_event:notify(nexchange_trading_book_eventmgr, {accept, Order}).


-spec notify_fill('order') -> ok.
notify_fill(Order) ->
  gen_event:notify(nexchange_trading_book_eventmgr, {full_fill, Order}).


-spec notify_partial_fill('order') -> ok.
notify_partial_fill(Order) ->
  gen_event:notify(nexchange_trading_book_eventmgr, {partial_fill, Order}).


-spec notify_rejection('order', any()) -> ok.
notify_rejection(Order, Reason) ->
  gen_event:notify(nexchange_trading_book_eventmgr, {rejection, Reason, Order}).


-spec notify_cancel('order', any()) -> ok.
notify_cancel(Order, Reason) ->
  gen_event:notify(nexchange_trading_book_eventmgr, {cancel, Reason, Order}).
