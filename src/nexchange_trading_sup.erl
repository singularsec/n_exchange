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

% creates a book for a symbol
create_book(Symbol) ->
  supervisor:start_child(nexchange_trading_book, [Symbol]),
  ok.

% Callback

init(_Args) ->
    Restart = {simple_one_for_one, 2, 5},

    ChildSpec = { nexchange_trading_book
         , {nexchange_trading_book,start_link,[]}
         , permanent
         , 200 % ms
         , worker
         , [nexchange_trading_book]
         },

    {ok, {Restart,[]}}.
