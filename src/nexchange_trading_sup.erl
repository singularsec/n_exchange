%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_trading_sup).

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


% Callback

init(_Args) ->
    
    Restart = {one_for_one, 2, 5},

    ChildSpec = { arbitrary_internal_name_term
         , {mod,func,args}
         , permanent
         , 200 % ms
         , worker
         , [mod]
         },

    {ok, {Restart,[]}}.

