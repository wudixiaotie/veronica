-module (veronica).

-export ([error/1, open/0, init/0, get/2, put/3, build_ring/0, locate/2]).

-record(ring, {size, interval, nodes, body}).

error(X) ->
    lager:error("[text] ~p~n", [X]).

open() ->
    {ok, Db} = erocksdb:open("data/test.db", [{create_if_missing, true}], []),
    Db.

put(Db, Key, Value) when
      is_binary(Key),
      is_binary(Value) ->
    erocksdb:put(Db, Key, Value, [{sync, true}]).

get(Db, Key) when
      is_binary(Key) ->
    erocksdb:get(Db, Key, [{verify_checksums, true}]).

init() ->
    Db = open(),
    Ring = build_ring(),
    RingBin = erlang:term_to_binary(Ring),
    put(Db, <<"ring">>, RingBin),
    ok.

build_ring() ->
    Size = 64,
    Max = math:pow(2, 256),
    Interval = Max / Size,
    Nodes = [erlang:node()|erlang:nodes()],
    Ring0 = #ring{size = Size, interval = Interval, nodes = Nodes},
    {ok, Body} = build_ring_body(Ring0),
    {ok, Ring0#ring{body = Body}}.

build_ring_body(Ring0) ->
    #ring{size = Size, interval = Interval, nodes = Nodes} = Ring0,
    N = erlang:trunc(Size / erlang:length(Nodes)),
    {ok, Body} = build_ring_body(N, Nodes, Interval, []),
    {ok, Body}.

build_ring_body(_, [], _, Body) ->
    {ok, Body};
build_ring_body(N, [Node|T], Interval, Body0) ->
    {ok, Body1} = do_build_ring_body(N, Node, Interval, Body0),
    build_ring_body(N, T, Interval, Body1).

do_build_ring_body(-1, _, _, Body) ->
    {ok, Body};
do_build_ring_body(N, Node, Interval, Body0) ->
    Body1 = [{N*Interval, Node}|Body0],
    do_build_ring_body(N - 1, Node, Interval, Body1).

locate(Ring, X) ->
    Hash = crypto:bytes_to_integer(crypto:hash(sha256, X)),
    #ring{interval = Interval, body = RingBody} = Ring,
    I = erlang:trunc(Hash / Interval),
    {V, _} = lists:nth(I + 1, RingBody),
    V.
