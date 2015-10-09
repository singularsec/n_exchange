-module(nexchange_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(rabbit_common),

    % case nexchange_sup:start_link() of
    Res = nexchange_sup_sup:start_link(),
    % nexchange_sup_sup:start_stuff(),
    Res.

stop(_State) ->
    ok.
