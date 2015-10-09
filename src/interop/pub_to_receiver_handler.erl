
-module(pub_to_receiver_handler).

-behavior(gen_event).

-include("../../include/secexchange.hrl").
-include("../../include/receiver_pb.hrl").

-compile(export_all).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

% Callback

% -record(pricechangeacksimplemessage, {
%     refid,
%     price,
%     quantity,
%     buyer,
%     seller,
%     direction,
%     netchgprevday,
%     variation,
%     volume,
%     condition, symbol
% }).

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
                                        price=Price,
                                        quantity=Qtd,
                                        buyer=l(Buyer),
                                        seller=l(Seller),
                                        direction=1,
                                        netchgprevday=1.0,
                                        variation=2.0,
                                        volume=2.0,
                                        condition=l("L") },

    Payload = receiver_pb:encode_pricechangeacksimplemessage(Rec),

    rabbitconnworker:send_to_receiver(Payload),

    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    error_logger:error_msg("Unexpected call on nexchange_fixsession_dispatcher_handler ~p ~n", [_Request]),
    {ok, unimplemented, State}.

handle_info(_Info, State) ->
    error_logger:error_msg("Unexpected eventinfo on nexchange_fixsession_dispatcher_handler ~p ~n", [_Info]),
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
