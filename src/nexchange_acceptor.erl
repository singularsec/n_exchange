%%% @doc Tcp acceptor
%%%


-module(nexchange_acceptor).

-behaviour(gen_server).

% API

-export([start_link/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { socket }).

% API

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link([Socket, Cb]) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Socket, []).

% Callback

init([Socket, Cb]) ->
  get_server:cast( self(), accept ),
  { ok, #state{socket=Socket} }.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(accept, #state{socket = Socket} = State) ->
  {ok, NewSocket} = gen_tcp:accept(Socket),

  % nexchange_fixsession_sup:create_session(NewSocket),

  % restart listening
  % gen_server:cast( self(), accept ),

	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
