-module(book_ev_handler).

-compile(export_all).

-behavior(gen_event).



% Callback

init(_Args) ->
  error_logger:info_msg("book_ev_handler is up ~p ~n", _Args),
  State = [],
  {ok, State}.

handle_event({Key, Order}, State) ->
  error_logger:info_msg("event arrived  ~p ~n", Key),
  NewList = [{Key, Order}] ++ State,
  {ok, NewList};

% handle_event({change_rejected, Order}, State) ->
%   % use SessionId to get fix session, send execution report to it
%   {ok, State};

% handle_event({order_rejected, Order}, State) ->
%   % use SessionId to get fix session, send execution report to it
%   {ok, State};
%
% handle_event({order_cancelled, Order}, State) ->
%   % use SessionId to get fix session, send execution report to it
%   {ok, State};
%
% handle_event({full_fill, QtdFilled, Order}, State) ->
%   % use SessionId to get fix session, send execution report to it
%   {ok, State};
%
% handle_event({partial_fill, QtdFilled, Order}, State) ->
%   % use SessionId to get fix session, send execution report to it
%   {ok, State};

handle_event(_Event, State) ->
  error_logger:info_msg("Ops ~p ~n", _Event),
	{stop, error, State}.

handle_call(get_all, State) ->
  {ok, State, State};

handle_call(_Request, State) ->
	{ok,unimplemented,State}.

handle_info(_Info, State) ->
	{ok,unimplemented,State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
