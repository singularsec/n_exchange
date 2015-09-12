-module(fix_message_handler).

-compile(export_all).

-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").



handle_messages([{#heartbeat{},_}|Messages], #state{} = State) ->
  % ?D(heartbeat),
  handle_messages(Messages, State);

handle_messages([{#logon{},_}|Messages], #state{} = State) ->

  handle_messages(Messages, State);


handle_messages([], State) ->
  {noreply, State}.
