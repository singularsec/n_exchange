-module(nexchange_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    Child = {sc_element, {sc_element, start_link, []},
              temporary, brutal_kill, worker, [sc_element]},
    Children = [Child],
    Strategy = {simple_one_for_one, 0, 1},

    %  {ok, { {one_for_one, 5, 10}, []} }.
    {ok, Strategy, Children}.
