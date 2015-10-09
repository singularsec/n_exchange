%%
%% Keeps a channel with a local rabbitmq
%%

-module(rabbitconnworker).

% -compile(export_all).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

% API

-export([start_link/0, stop/0, send_to_receiver/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {connection, channel}).


start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

send_to_receiver(Payload) ->
    gen_server:cast(?MODULE, {send_to_receiver, Payload}).

% Callback

init(_Args) ->
    {ok, host}  = application:get_env(nexchange, rabbitmq_host),
    {ok, vhost} = application:get_env(nexchange, rabbitmq_vhost),
    {ok, user}  = application:get_env(nexchange, rabbitmq_user),
    {ok, pwd}   = application:get_env(nexchange, rabbitmq_pwd),

    Params = #amqp_params_network{username     = user,
                                  password     = pwd,
                                  virtual_host = vhost,
                                  host         = host,
                                  port         = 5672
                                  },
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(Params),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),

    {ok, #state{connection=Connection, channel=Channel}}.

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(Request, _From, State) ->
    error_logger:info_msg("handle_call for ?? message ~p ~n", Request),
    {stop, unimplemented, State}.

handle_cast({send_to_receiver, Payload}, State = #state{channel = Channel}) ->
    Publish = #'basic.publish'{
        exchange = <<"clear.e.receiver">>,
        routing_key = <<"receiver-s">>
    },

    % -record('P_basic', {
    %    content_type, content_encoding, headers, delivery_mode,
    %    priority, correlation_id, reply_to,
    %    expiration, message_id, timestamp, type, user_id, app_id, cluster_id}).

    % http://stackoverflow.com/questions/19408705/how-to-use-message-headers-in-rabbitmqs-erlang-client
    StringHeader = {<<"protobuf">>, short, 1},

    Props = #'P_basic'{
        delivery_mode = 2,
        headers = [StringHeader],
        type = <<"Clear.Foundation.Quotes.Messages.PriceChangeAckSimpleMessage, Clear.Foundation.Quotes.Messages">>
    },

    amqp_channel:cast(Channel, Publish, #amqp_msg{props=Props, payload = Payload}),
    {noreply, State};

handle_cast(Request, State) ->
    error_logger:info_msg("handle_cast for ?? message ~p ~n", Request),
    {stop, unimplemented, State}.

handle_info(_Info, State) ->
    % {stop, unimplemented, State}.
    {noreply, State}.

terminate(_Reason, State = #state{connection=Connection, channel=Channel}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
