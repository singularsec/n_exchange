
-module(nexchange_book_tests).

-compile(export_all).

-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

something_test() ->
  % error_logger:error_report("test"),
  Book = nexchange_book:create(),
  % insert_sell(#order{price = Price, time = _T, qtd = Qtd, id = Id} = Order, Book) ->

  Key = nexchange_book:insert_sell(#order{price = 10, time = 0, qtd = 100, id = "t"}, Book),

  error_logger:info_msg("Key is ~p ~n", [Key]).
