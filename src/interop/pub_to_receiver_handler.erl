
-module(pub_to_receiver_handler).

-behavior(gen_event).

-include("../../include/secexchange.hrl").
-include("../../include/receiver_pb.hrl").

-compile(export_all).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

% Callback

% simple_pb:encode_person({person, <<"Nick">>, <<"Mountain View">>,
%     <<"+1 (000) 555-1234">>,25, undefined}).
% https://github.com/basho/erlang_protobuffs

init(_Args) ->
    State = {},
    {ok, State}.

handle_event({new_trade, #tradeinfo{symbol=Symbol, refid=Id, price=Price,
                                    qtd=Qtd, buyer=Buyer, seller=Seller}},
             State) ->

    Rec = #pricechangeacksimplemessage{ symbol=l(Symbol),
                                        refid=l(Id),
                                        price=Price / 10000.0,
                                        quantity=Qtd / 1.0,
                                        buyer=l(Buyer),
                                        seller=l(Seller),
                                        direction='Up',
                                        netchgprevday=1.0,
                                        variation=2.0,
                                        volume=2.0,
                                        condition=l("L") },

    % error_logger:error_msg("record ~p ~n", [Rec]),

    % stupid protobuffs implementation doesnt accept records
    Payload = receiver_pb2:encode_pricechangeacksimplemessage(Rec),

    rabbitconnworker:send_to_receiver(iolist_to_binary(Payload)),

    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    error_logger:error_msg("Unexpected call on pub_to_receiver_handler ~p ~n", [_Request]),
    {ok, unimplemented, State}.

handle_info(_Info, State) ->
    error_logger:error_msg("Unexpected eventinfo on pub_to_receiver_handler ~p ~n", [_Info]),
    {ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


l(V) when is_integer(V) ->
    l(integer_to_list(V));

l(V) when is_list(V) ->
    list_to_binary(V);

l(V) when is_binary(V) -> V.
