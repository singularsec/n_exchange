%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_acceptor_sup).

-behaviour(supervisor).

% API

-export([start_link/1, create_acceptor/0]).

% Callback

-export([init/1]).



% API

%% @doc Short description.
-spec start_link(term())->{ok,pid()}|ignore|{error,any()}.
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

create_acceptor() ->
  supervisor:start_child(?MODULE, []).

% Callback

init(Args) ->
  Callback = Args,

  {ok, Listener} = gen_tcp:listen(8000, [binary, {active, true}]),

  spawn_link(fun start_listeners/0),
  % spawn_link(fun () -> [ create_acceptor() || _ <- lists:seq(1, 1)], ok  end),

  Restart = {simple_one_for_one, 2, 5},

  ChildSpec = {nexchange_acceptor
       , {nexchange_acceptor, start_link, [Listener, Callback]}
       , permanent
       , 200 % ms
       , worker
       , [nexchange_acceptor]
       },

  {ok, {Restart,[ChildSpec]}}.

start_listeners() ->
  create_acceptor(), ok.
