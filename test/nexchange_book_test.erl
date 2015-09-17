-module(nexchange_book_test).

-compile(export_all).

-include("../include/secexchange.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
