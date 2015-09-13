
-module(nexchange_bookregistry).
-behaviour(gen_server).

% API

-export([start_link/0]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% -record(state, { dict }).

% API

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

% Callback

init(_Args) ->
    State = dict:new(),
    {ok, State}.

handle_call({get_or_create_book, Symbol}, _From, State) ->
  {Pid, NewState} = case dict:is_key(Symbol, State) of
    true ->
      [RegPid] = dict:fetch(Symbol, State),
      {RegPid, State};
    false ->
      {ok, Child} = nexchange_trading_sup:create_book(Symbol),
      {Child, dict:append(Symbol, Child, State)}
  end,
	{reply, Pid, NewState};

handle_call(_Request, _From, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_cast(_Request, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_info(_Info, State) ->
	{stop, unimplemented, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
