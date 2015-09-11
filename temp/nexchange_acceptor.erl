-module(nexchange_acceptor).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

% -- API

start_link(LSocket) ->
  gen_server:start_link(?MODULE, LSocket, []).


% -- Callbacks

init(Socket) ->
  get_server:cast( self(), accept ),
  { ok, #state{socket=Socket} }.


handle_call(_E, _From, State) ->
  {noreply, State}.


handle_cast(accept, #state{socket=ListenSocket} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),

  % restart
  gen_server:cast( self(), accept ),

  {noreply, State}.


handle_info(_E, State) -> {noreply, State}.

code_change(_,_,_) -> ok.

terminate(_,_) -> ok.
