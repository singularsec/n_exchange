%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_fixsession_sup).

-behaviour(supervisor).

% API

-export([start_link/1]).

% Callback

-export([init/1]).


% API

%% @doc Short description.
-spec start_link(term())->{ok,pid()}|ignore|{error,any()}.
start_link(Args) ->
  supervisor:start_link(?MODULE, Args).


% runs in the context of acceptor
create_session(Socket) ->
  {ok, Child} = supervisor:start_child(?MODULE, [Socket]),
  {ok, _} = gen_tcp:controlling_process(Socket, Child),
  {ok, Child}.


% Callback

init(Args) ->
  [Socket] = Args,

  ChildSpec = {nexchange_fixsession
       , {nexchange_fixsession, start_link, [Socket]}
       , temporary
       , 200 % ms
       , worker
       , [nexchange_fixsession]
       },

  Restart = {simple_one_for_one, 2, 5},

  {ok, {Restart,[ChildSpec]}}.
