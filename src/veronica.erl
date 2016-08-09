-module (veronica).

-export ([error/1, open/0]).

error(X) ->
    lager:error("[text] ~p~n", [X]).

open() ->
    {ok, Db} = erocksdb:open("data/test.db", [{create_if_missing, true}], []),
    Db.