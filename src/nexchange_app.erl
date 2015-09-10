-module(nexchange_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % case nexchange_sup:start_link() of
    nexchange_sup_sup:start_link().

stop(_State) ->
    ok.
