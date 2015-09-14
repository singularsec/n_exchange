-module(nexchange_sessionregistry).

-behaviour(gen_server).

% API

-export([start_link/0, register_fixsession/2, unregister_fixsession/2, get_fixsessions/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API

-spec register_fixsession(any(), pid()) -> ok.
register_fixsession(SessionId, Pid) when is_pid(Pid) ->
  gen_server:cast(nexchange_sessionregistry, {register, SessionId, Pid}).

-spec unregister_fixsession(any(), pid()) -> ok.
unregister_fixsession(SessionId, Pid) when is_pid(Pid) ->
  gen_server:cast(nexchange_sessionregistry, {unregister, SessionId, Pid}).

-spec get_fixsessions(any()) -> [pid()].
get_fixsessions(SessionId) ->
  Pids = gen_server:call(nexchange_sessionregistry, {get_sessions, SessionId}, 1000),
  Pids.


% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

% Callback

init(_Args) ->
    State = dict:new(),
    {ok, State}.

handle_call({get_sessions, SessionId}, _From, State) ->
  Sessions = case dict:is_key(SessionId, State) of
    true ->
      % TODO: process_info/2 to check if PID is alive?
      dict:fetch(SessionId, State);
    false -> {[], State}
  end,
	{reply, Sessions, State};

handle_call(_Request, _From, State) ->
	{stop, unimplemented, State}.

handle_cast({register, Session, Pid}, State) ->
  NewDict = dict:append(Session, Pid, State),
  {noreply, NewDict};

handle_cast({unregister, Session, Pid}, State) ->
  NewDict = case dict:is_key(Session, State) of
    true ->
      Values = dict:fetch(Session, State),
      NewList = lists:delete(Pid, Values),
      dict:store(Session, NewList, State);
    false -> State
  end,
  {noreply, NewDict};

handle_cast(_Request, State) ->
  % TODO: support for ops: buy/sell/etc
	{stop, unimplemented, State}.

handle_info(_Info, State) ->
	{stop, unimplemented, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
