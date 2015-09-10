-module(nexchange_sup_sup).

-behaviour(supervisor).

-export([start_link/0, on_accepted_socket/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, nexchange_sup_sup}, ?MODULE, []).

% start_pool(Name, Limit, MFA) ->
%     ChildSpec = {Name,
%                  {sup1, start_link, [Name, Limit, MFA]},
%                   permanent, 10500, supervisor, [sup1]},
%     supervisor:start_child(nexchange_sup_sup, ChildSpec).

% stop_pool(Name) ->
%     supervisor:terminate_child(nexchange_sup_sup, Name),
%     supervisor:delete_child(nexchange_sup_sup, Name).


% runs in the context of nexchange_acceptor_sup so it's safe to transfer ownership of socket
on_accepted_socket(Socket) ->
  % todo create fix session, transfer socket to new worker
  Session = nexchange_fixsession_sup:create_session(Socket),

  ok.

init([]) ->
  TcpAcceptorSpec = {nexchange_acceptor_sup, 
                      {nexchange_acceptor_sup, start_link, [ on_accepted_socket ]}, 
                      permanent, 10000, supervisor, [nexchange_acceptor_sup]},

  FixSupSpec      = {nexchange_fixsession_sup, 
                      {nexchange_fixsession_sup, start_link, []}, 
                      permanent, 10000, supervisor, [nexchange_fixsession_sup]},

  TradingSupSpec  = {nexchange_trading_sup, 
                      {nexchange_trading_sup, start_link, []}, 
                      permanent, 10000, supervisor, [nexchange_trading_sup]},

  MaxRestart = 6,
  MaxTime = 3000,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.


