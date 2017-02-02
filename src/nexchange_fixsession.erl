%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_fixsession).

-behaviour(gen_server).

-include("log.hrl").
-include("../include/fix_session.hrl").
-include("../include/admin44.hrl").
-include("../include/business44.hrl").
-include("../include/secexchange.hrl").


% API

-export([start_link/1]).

% Callback

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% API

% -spec start_link(any())->{ok,pid()} | ignore | {error,any()}.
start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

% Callback

init(Socket) ->
  EmptyBuffer = <<>>,
  State = #state{socket=Socket,
                 their_seq=1,
                 our_seq=1,
                 authenticated=false,
                 prevbuffer=EmptyBuffer},
  {ok, State}.

handle_call(_Request, _From, State) ->
	{stop, unimplemented, State}.


handle_cast({send_heartbeat, Fields}, #state{our_seq=Seq} = State) ->
  ?DBG("sending heartbeat ~p ~n", [Seq]),
  NewState = fix_message_handler:send(heartbeat, [], Fields, State),
  {noreply, NewState};


handle_cast({send, #execreport{} = Report},
            #state{socket=Socket, our_seq=Seq} = State) ->

  Bin = exec_report:report_to_fix_bin(Report, Seq),
  gen_tcp:send(Socket, Bin),

  error_logger:info_msg("sending exec report ~p ~n", [fix:dump(Bin)]),

  % error_logger:info_msg("sending exec report done ~p ~n", [Bin]),
  {noreply, State#state{our_seq=Seq+1}};


handle_cast({send, #cancelreject{} = Report},
            #state{socket=Socket, our_seq=Seq} = State) ->

  error_logger:info_msg("sending cancel reject report"), %  ~p ~n", [Report]),

  Bin = exec_report:report_to_fix_bin(Report, Seq),
  gen_tcp:send(Socket, Bin),

  % error_logger:info_msg("sending exec report done ~p ~n", [Bin]),
  {noreply, State#state{our_seq=Seq+1}};

handle_cast(_Request, State) ->
  error_logger:info_msg("UPS -- unimplemented! --"), %  ~p ~n", [Report]),
	{stop, unimplemented, State}.

handle_info({tcp, _Socket, Data}, #state{authenticated=false,prevbuffer=PrevBuf} = State) ->
  Buffer = <<PrevBuf/binary, Data/binary>>,

  {Messages, Rest} = fix_connection:decode_messages(Buffer),

  fix_message_handler:handle_messages(Messages, Rest, State#state{prevbuffer=Buffer});


handle_info({tcp, _Socket, Data}, #state{authenticated=true,prevbuffer=PrevBuf} = State) ->
  Buffer = <<PrevBuf/binary, Data/binary>>,
  % error_logger:info_msg("received something from socket ~p ~n", [Data]),

  {Messages, Rest} = fix_connection:decode_messages(Buffer),

  fix_message_handler:handle_messages(Messages, Rest, State#state{prevbuffer=Buffer});


handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
	{stop, unimplemented, State}.


terminate(_Reason, #state{sessionid=Sender} = State) ->
  nexchange_fixsession_eventmgr:notify_session_terminated(Sender, self()),

	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


% Internal implementation
