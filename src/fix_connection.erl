-module(fix_connection).

% -compile(export_all).

-export([decode_messages/1]).

-include("log.hrl").


% Public api

decode_messages(Bin) ->
  decode_messages(Bin, []).

decode_messages(Bin, Acc) ->
  case fix0:decode(Bin) of
    {ok, Message, MessageBin, Rest} ->
      % Continue decoding
      decode_messages(Rest, [{Message,MessageBin}|Acc]);
    {more, _} ->
      return_decoded(lists:reverse(Acc), Bin);
    error ->
      ?D({error, Bin}),
      erlang:error(broken_fix)
  end.

% Internal api

return_decoded(Messages, Rest) ->
  {Messages, Rest}.
