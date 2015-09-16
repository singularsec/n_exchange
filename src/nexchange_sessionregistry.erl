-module(nexchange_sessionregistry).

-behaviour(gen_server).

% API

-export([start_link/0, stop/0, register_fixsession/2, unregister_fixsession/2,
         get_fixsessions/1, get_registered/0]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API

-spec register_fixsession(any(), pid()) -> ok.
register_fixsession(SessionId, Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {register, SessionId, Pid}).

-spec unregister_fixsession(any(), pid()) -> ok.
unregister_fixsession(SessionId, Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {unregister, SessionId, Pid}).

-spec get_fixsessions(any()) -> [pid()].
get_fixsessions(SessionId) ->
  Pids = gen_server:call(?MODULE, {get_sessions, SessionId}, 1000),
  Pids.

get_registered() ->
  gen_server:call(?MODULE, get_registered).


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

handle_call(get_registered, _From, State) ->
  Keys = dict:fetch_keys(State),
  {reply, Keys, State};

handle_call({get_sessions, SessionId}, _From, State) ->
  Sessions = case dict:is_key(SessionId, State) of
    true ->
      % TODO: process_info/2 to check if PID is alive?
      dict:fetch(SessionId, State);
    false -> []
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
      case length(NewList) of
        0 -> dict:erase(Session, State);
        _ -> dict:store(Session, NewList, State)
      end;
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
