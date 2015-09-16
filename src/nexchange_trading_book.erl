%%% @doc One line blurb.
%%%

-module(nexchange_trading_book).

-behaviour(gen_server).

% API

-export([start_link/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { book, symbol }).

% API

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link(Symbol) ->
  BookName = list_to_atom( atom_to_list(?MODULE) ++ "_" ++ Symbol ),
  gen_server:start_link({local, BookName}, ?MODULE, Symbol, []).


% Callback

init(Symbol) ->
  Book = nexchange_book:create(Symbol),
  State = #state{book = Book, symbol = Symbol},
  {ok, State}.

handle_call(_Request, _From, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_cast(_Request, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_info(_Info, State) ->
	{stop, unimplemented, State}.

terminate(_Reason, #state{symbol = Symbol} = State) ->
  % unregister this pid so we dont keep a zombie one
  nexchange_bookregistry:book_removed(Symbol, self()),
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
