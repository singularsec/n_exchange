-module(nexchange_fixsession_registry_handler).

-behavior(gen_event).

% Callback

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

% Callback

init(_Args) ->
    State = {},
    {ok, State}.

handle_event({session_authenticated, SessionId, Pid}, State) ->
  nexchange_sessionregistry:register_fixsession(SessionId, Pid),
	{ok, State};

handle_event({session_terminated, SessionId, Pid}, State) ->
  nexchange_sessionregistry:unregister_fixsession(SessionId, Pid),
	{ok, State};

handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok,unimplemented,State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
