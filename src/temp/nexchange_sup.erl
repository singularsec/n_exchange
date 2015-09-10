-module(nexchange_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_acceptor/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_acceptor() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Listener} = gen_tcp:listen(8000, [binary, {active, true}]),

    spawn_link(fun () -> [ start_acceptor() || _ <- lists:seq(1, 5)], ok  end),

    Child = {acceptor, {nexchange_acceptor, start_link, [Listener]},
              temporary, 1000, worker, [nexchange_acceptor]},
    Children = [Child],
    Strategy = {simple_one_for_one, 60, 3600},

    %  {ok, { {one_for_one, 5, 10}, []} }.
    {ok, Strategy, Children}.
