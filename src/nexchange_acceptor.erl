%%% @doc Tcp acceptor
%%%


-module(nexchange_acceptor).

-behaviour(gen_server).

% API

-export([start_link/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(ate, { field = undefined :: any() }).

% API

-spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link(Args) -> 
	gen_server:start_link(?MODULE, Args, []).

% Callback

init(_Args) ->
    State = #ate{
        field = undefined
    },
    {ok, State}.

handle_call(_Request, _From, State) -> 
	{stop, unimplemented, State}.

handle_cast(_Request, State) -> 
	{stop, unimplemented, State}.

handle_info(_Info, State) -> 
	{stop, unimplemented, State}.

terminate(_Reason, State) -> 
	{ok, State}.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

