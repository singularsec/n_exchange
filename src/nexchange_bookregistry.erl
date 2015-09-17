
-module(nexchange_bookregistry).

-behaviour(gen_server).

% API

-export([start_link/0, stop/0, get_book/1, book_removed/2, get_registered/0]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


% API

-spec get_book(any()) -> pid() | {error, any()}.
get_book(Symbol) ->
  BookPid = gen_server:call(?MODULE, {get_or_create_book, Symbol}), %, 1000),
  BookPid.

-spec get_registered() -> ok | {error, any()}.
get_registered() ->
  gen_server:call(?MODULE, get_books).

-spec book_removed(any(), pid()) -> ok | {error, any()}.
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
      {RegPid, State};
    false ->
      Child = nexchange_trading_sup:create_book(Symbol),
      NewS = dict:append(Symbol, Child, State),
      {Child, NewS}
  end,
  {reply, Pid, NewState};

handle_call(Request, _From, State) ->
  error_logger:info_msg("handle_call for ?? message ~p ~n", Request),
	{stop, unimplemented, State}.

handle_cast({book_removed, Symbol, _Pid}, State) ->
  NewState = dict:erase(Symbol, State),
  {noreply, NewState};

handle_cast(Request, State) ->
  error_logger:info_msg("handle_cast for ?? message ~p ~n", Request),
	{stop, unimplemented, State}.

handle_info(_Info, State) ->
	% {stop, unimplemented, State}.
  {noreply, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
