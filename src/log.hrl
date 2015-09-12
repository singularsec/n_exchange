% -define(D(X), io:format("~p:~p ~240p~n", [?MODULE, ?LINE, X])).
% -define(DBG(Fmt,X), io:format("~p:~p "++Fmt++"~n", [?MODULE, ?LINE | X])).

-define(D(X), error_logger:info_msg("~p:~p ~240p~n", [?MODULE, ?LINE, X])).
-define(DBG(Fmt,X), error_logger:info_msg("~p:~p "++Fmt++"~n", [?MODULE, ?LINE | X])).
