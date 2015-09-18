-module(nexchange_fixsession_dispatcher_handler).

-behavior(gen_event).

-include("../include/secexchange.hrl").

% Callback

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

% Callback

init(_Args) ->
    State = {},
    {ok, State}.

handle_event({accept, Order}, State) ->
  % use SessionId to get fix session, send execution report to it
  Report = exec_report:build_accept(Order),
  exec_report_dispatcher:dispatch(Report, Order),
	{ok, State};

handle_event({full_fill, Order}, State) ->
  Report = exec_report:build_full_fill(Order),
  exec_report_dispatcher:dispatch(Report, Order),
	{ok, State};

handle_event({partial_fill, Order}, State) ->
  Report = exec_report:build_partial_fill(Order),
  exec_report_dispatcher:dispatch(Report, Order),
	{ok, State};

handle_event({rejection, {Order, Reason}}, State) ->
  Report = exec_report:build_rejection(Order, Reason),
  exec_report_dispatcher:dispatch(Report, Order),
	{ok, State};

handle_event({cancel, {Order, Reason}}, State) ->
  Report = exec_report:build_cancel(Order, Reason),
  exec_report_dispatcher:dispatch(Report, Order),
	{ok, State};

handle_event(_Event, State) ->
  error_logger:error_msg("Unexpected event on nexchange_fixsession_dispatcher_handler ~p ~n", [_Event]),
	{stop, unimplemented, State}.

handle_call(_Request, State) ->
  error_logger:error_msg("Unexpected call on nexchange_fixsession_dispatcher_handler ~p ~n", [_Request]),
	{ok, unimplemented, State}.

handle_info(_Info, State) ->
  error_logger:error_msg("Unexpected eventinfo on nexchange_fixsession_dispatcher_handler ~p ~n", [_Info]),
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
