
-module(pub_to_receiver).

% -include("../../include/receiver_pb.hrl").
-compile(export_all).

% simple_pb:encode_person({person, <<"Nick">>, <<"Mountain View">>,
%     <<"+1 (000) 555-1234">>,25, undefined}).

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
%     condition
% }).

% https://github.com/basho/erlang_protobuffs

setup() ->
    ok.

encode_and_send() ->
  % Rec = #pricechangeacksimplemessage{ refid=1, price=1.0, quantity=2.0,
  %                                     buyer="x", seller="y",
  %                                     direction=1, netchgprevday=1.0,
  %                                     variation=2.0, volume=2.0, condition="L" },
  % Bin = receiver_pb:encode_pricechangeacksimplemessage(Rec),

  % https://www.rabbitmq.com/erlang-client-user-guide.html
  % Payload = Bin,

  % Publish = #'basic.publish'{exchange = X, routing_key = Key},
  % amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

  ok.
