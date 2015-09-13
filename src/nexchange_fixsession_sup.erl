%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_fixsession_sup).

-behaviour(supervisor).

% API

-export([start_link/0, create_session/1]).

% Callback

-export([init/1]).


% API

%% @doc Short description.
% -spec start_link(term())->{ok,pid()}|ignore|{error,any()}.
start_link() ->
  % supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% runs in the context of acceptor
create_session(Socket) ->
  {ok, Child} = supervisor:start_child(?MODULE, [Socket]),
  ok = gen_tcp:controlling_process(Socket, Child),
  {ok, Child}.

% Callback

init(_Args) ->
  % [Socket] = Args,

  ChildSpec = {nexchange_fixsession
       , {nexchange_fixsession, start_link, []}
       , temporary
       , 200 % ms
       , worker
       , [nexchange_fixsession]
       },

  Restart = {simple_one_for_one, 2, 5},

  {ok, {Restart,[ChildSpec]}}.
