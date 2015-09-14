-module(nexchange_sup_sup).

-behaviour(supervisor).

-export([start_link/0, on_accepted_socket/1]).
-export([init/1]).

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  ok = nexchange_fixsession_eventmgr:setup_handlers(),
  {ok, Pid}.

% runs in the context of nexchange_acceptor_sup so it's safe to transfer ownership of socket
on_accepted_socket(Socket) ->
  % todo create fix session, transfer socket to new worker
  % _Session = nexchange_fixsession_sup:create_session(Socket),
  error_logger:info_msg("on_accepted_socket invoked ~p ~n", Socket),
  nexchange_fixsession_sup:create_session(Socket),
  ok.

% start_stuff() ->
%   nexchange_acceptor_sup:start_listeners().

init(_Args) ->
  OnAcceptCallback = {?MODULE, on_accepted_socket},

  ShutdownTime = 10000,

  FixSessionEventMgr = {nexchange_fixsession_eventmgr,
                      {gen_event, start_link, [{local, nexchange_fixsession_eventmgr}]},
                      permanent, ShutdownTime, worker, [dynamic]},

  TcpAcceptorSpec = {nexchange_acceptor_sup,
                      {nexchange_acceptor_sup, start_link, [ OnAcceptCallback ]},
                      permanent, ShutdownTime, supervisor, [nexchange_acceptor_sup]},

  FixSupSpec      = {nexchange_fixsession_sup,
                      {nexchange_fixsession_sup, start_link, []},
                      permanent, ShutdownTime, supervisor, [nexchange_fixsession_sup]},

  TradingSupSpec  = {nexchange_trading_sup,
                      {nexchange_trading_sup, start_link, []},
                      permanent, ShutdownTime, supervisor, [nexchange_trading_sup]},

  BookRegistrySpec = {nexchange_bookregistry,
                      {nexchange_bookregistry, start_link, []},
                      permanent, ShutdownTime, worker, [nexchange_bookregistry]},

  SessionRegistrySpec = {nexchange_sessionregistry,
                      {nexchange_sessionregistry, start_link, []},
                      permanent, ShutdownTime, worker, [nexchange_sessionregistry]},

  MaxRestart = 6,
  MaxTime = 3000,

  Specs = [FixSessionEventMgr,
           SessionRegistrySpec,
           BookRegistrySpec,
           FixSupSpec,
           TcpAcceptorSpec,
           TradingSupSpec],

  {ok, {{one_for_one, MaxRestart, MaxTime}, Specs}}.
