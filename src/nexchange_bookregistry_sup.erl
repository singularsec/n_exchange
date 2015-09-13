%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.

-module(nexchange_bookregistry_sup).

-behaviour(supervisor).

% API

-export([start_link/0, send_to_book/2]).

% Callback

-export([init/1]).

% API

%% @doc Short description.
% -spec start_link()->{ok,pid()}|ignore|{error,any()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% creates a book for a symbol
send_to_book(Symbol, Message) ->
  BookPid = gen_server:call(nexchange_bookregistry, {get_or_create_book, Symbol}, 1000),
  case BookPid of
    BookPid when is_pid(BookPid) ->
      gen_server:call(BookPid, Message);
    _ -> {error, book_registry_error, BookPid}
  end.

% Callback

init(_Args) ->
    Restart = {one_for_one, 2, 5},

    ChildSpec = { nexchange_bookregistry
         , {nexchange_bookregistry, start_link, []}
         , permanent
         , 200 % ms
         , worker
         , [nexchange_bookregistry]
         },

    {ok, {Restart,[ChildSpec]}}.
