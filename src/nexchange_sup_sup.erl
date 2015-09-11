-module(nexchange_sup_sup).

-behaviour(supervisor).

-export([start_link/0, on_accepted_socket/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% runs in the context of nexchange_acceptor_sup so it's safe to transfer ownership of socket
on_accepted_socket(Socket) ->
  % todo create fix session, transfer socket to new worker
  % _Session = nexchange_fixsession_sup:create_session(Socket),
  error_logger:info_msg("on_accepted_socket invoked ~p ~n", Socket),
  ok.

% start_stuff() ->
%   nexchange_acceptor_sup:start_listeners().

init(_Args) ->
  OnAcceptCallback = {?MODULE, on_accepted_socket},

  TcpAcceptorSpec = {nexchange_acceptor_sup,
                      {nexchange_acceptor_sup, start_link, [ OnAcceptCallback ]},
                      permanent, 10000, supervisor, [nexchange_acceptor_sup]},

  FixSupSpec      = {nexchange_fixsession_sup,
                      {nexchange_fixsession_sup, start_link, []},
                      permanent, 10000, supervisor, [nexchange_fixsession_sup]},

  TradingSupSpec  = {nexchange_trading_sup,
                      {nexchange_trading_sup, start_link, []},
                      permanent, 10000, supervisor, [nexchange_trading_sup]},

  MaxRestart = 6,
  MaxTime = 3000,

  %TradingSupSpec,FixSupSpec,
  Specs = [TcpAcceptorSpec],

  {ok, {{one_for_one, MaxRestart, MaxTime}, Specs}}.
