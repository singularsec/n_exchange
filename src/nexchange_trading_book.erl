-module(nexchange_trading_book).

-behaviour(gen_server).

-include("../include/secexchange.hrl").

% API

-export([start_link/1]).
-export([ping/1, send_new_order_single/2, modify_order/2, try_cancel/2, cancel_order/2, dump_book/1, qa_fillbook/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { book, symbol }).

% API

ping(BookPid) when is_pid(BookPid) ->
  gen_server:call(BookPid, ping).

send_new_order_single(BookPid, #order{} = Order) when is_pid(BookPid) ->
  gen_server:cast(BookPid, {new_order_single, Order}).

modify_order(BookPid, #order{} = Order) when is_pid(BookPid) ->
  gen_server:cast(BookPid, {change_order, Order}).

try_cancel(BookPid, #order_cancel{} = Order) when is_pid(BookPid) ->
  gen_server:call(BookPid, {try_cancel_order, Order}).

cancel_order(BookPid, #order{} = Order) when is_pid(BookPid) ->
  gen_server:cast(BookPid, {cancel_order, Order}).

dump_book(BookPid) when is_pid(BookPid) ->
  gen_server:call(BookPid, dump_book).

qa_fillbook(BookPid) when is_pid(BookPid) ->
  gen_server:call(BookPid, qa_fillbook).

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link(Symbol) ->
  BookName = list_to_atom( atom_to_list(?MODULE) ++ "_" ++ Symbol ),
  gen_server:start_link({local, BookName}, ?MODULE, Symbol, []).


% Callback

init(Symbol) ->
  Book = n_orderbook:create(Symbol),
  State = #state{book = Book, symbol = Symbol},
  {ok, State}.

handle_call(ping, _From, State) ->
	{reply, pong, State};

handle_call(dump_book, _From, #state{book=Book} = State) ->
  Reply = n_orderbook:dump(Book),
  {reply, Reply, State};

handle_call(qa_fillbook, _From, #state{book=Book} = State) ->
  Reply = n_orderbook:qa_fillbook(Book),
  {reply, Reply, State};

handle_call({try_cancel_order, #order_cancel{} = Order}, _From, #state{book=Book} = State) ->
  Reply = n_orderbook:try_cancel_order(Order, Book),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
	{stop, unimplemented, State}.

handle_cast({new_order_single, #order{} = Order}, #state{book=Book} = State) ->
  n_orderbook:add_new_order_single(Order, Book),
	{noreply, State};

handle_cast({change_order, #order{} = Order}, #state{book=Book} = State) ->
  n_orderbook:change_order(Order, Book),
  {noreply, State};

handle_cast({cancel_order, #order{} = Order}, #state{book=Book} = State) ->
  n_orderbook:cancel_order(Order, Book),
  {noreply, State};

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
