%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_acceptor_sup).

-behaviour(supervisor).

% API

-export([start_link/1, create_acceptor/0, start_listeners/0]).

% Callback

-export([init/1]).

% API

%% @doc Short description.
% -spec start_link(term())->{ok,pid()}|ignore|{error,any()}.
start_link(Args) ->
  Res = supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
  % error_logger:info_msg("start_link1 ret ~p ~n", [Res]),
  Res.

create_acceptor() ->
  Res = supervisor:start_child(?MODULE, []),
  error_logger:info_msg("start_child ret ~p ~n", [Res]),
  Res.

% Callback

init(Args) ->
  Callback = Args,

  {ok, Listener} = gen_tcp:listen(8000, [binary, {active, true}]),

  Restart = {simple_one_for_one, 60, 3600},

  % NArgs = [Listener, Callback]
  ChildSpec = {nexchange_tcpacc, {nexchange_acceptor, start_link, [Listener,Callback]},
                        transient, 10000, worker, [nexchange_acceptor]},

  spawn_link(fun start_listeners/0),
  % spawn_link(fun () -> [ create_acceptor() || _ <- lists:seq(1, 1)], ok  end),

  % error_logger:info_msg("init done with spec ~p ~n", [ChildSpec]),

  {ok, {Restart,[ChildSpec]}}.

start_listeners() ->
  [create_acceptor() || _ <- lists:seq(1,1) ], ok.
