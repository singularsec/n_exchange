-module(nexchange_session_sup).

%% API
-export([start_link/0, start_fix_session/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_fix_session() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ok.