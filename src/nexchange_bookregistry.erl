
-module(nexchange_bookregistry).

-behaviour(gen_server).

% API

-export([start_link/0, stop/0, get_book/1, send_to_book/2, book_removed/2, get_registered/0]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% -record(state, { dict }).

% API

% -spec(get_book) -> pid()
get_book(Symbol) ->
  BookPid = gen_server:call(?MODULE, {get_or_create_book, Symbol}, 1000),
  BookPid.

send_to_book(Symbol, Message) ->
  BookPid = get_book(Symbol),
  case BookPid of
    BookPid when is_pid(BookPid) ->
      gen_server:call(BookPid, Message);
    _ -> {error, book_registry_error, BookPid}
  end.

get_registered() ->
  gen_server:call(?MODULE, get_books).

book_removed(Symbol, Pid) ->
  gen_server:cast(?MODULE, {book_removed, Symbol, Pid}).

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

% Callback

init(_Args) ->
    State = dict:new(),
    {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, State};

handle_call(get_books, _From, State) ->
  Reply = dict:fetch_keys(State),
  {reply, Reply, State};

handle_call({get_or_create_book, Symbol}, _From, State) ->
  {Pid, NewState} = case dict:is_key(Symbol, State) of
    true ->
      [RegPid] = dict:fetch(Symbol, State),
      % TODO: process_info/2 to check if PID is alive?
      {RegPid, State};
    false ->
      {ok, Child} = nexchange_trading_sup:create_book(Symbol),
      {Child, dict:append(Symbol, Child, State)}
  end,
	{reply, Pid, NewState};

handle_call(_Request, _From, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_cast({book_removed, Symbol, _Pid}, State) ->
  NewState = dict:erase(Symbol, State),
  {noreply, NewState};

handle_cast(_Request, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_info(_Info, State) ->
	{stop, unimplemented, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
