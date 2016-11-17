-module (veronica).

-export ([error/1, open/0, init/0, get/2, put/3, build_ring/0, locate/2]).

-record(ring, {size,
               plength, %% partition length
               nodes,
               plan,
               body}).

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
    PLength = erlang:trunc(Max / Size),
    Nodes = [erlang:node()|erlang:nodes()],
    SortedNodes = lists:sort(Nodes),
    Ring0 = #ring{size = Size,
                  plength = PLength,
                  nodes = SortedNodes},
    %% {ok, Ring1} = build_ring_plan(Ring0),
    {ok, Ring} = build_ring_body(Ring0),
    {ok, Ring}.

build_ring_body(#ring{size = Size,
                      plength = PLength,
                      nodes = Nodes} = Ring) ->
    NodesLength = erlang:length(Nodes),
    {ok, Body} = build_ring_body(Size, NodesLength, Nodes, PLength, 0, 0, []),
    {ok, Ring#ring{body = Body}}.

build_ring_body(Size, _, _, _, Size, _, Body) ->
    {ok, lists:reverse(Body)};
build_ring_body(Size, NodesLength, Nodes, PLength, N, I0, Body0 = [{PHead0, _}|_]) ->
    I =
        case I0 of
            NodesLength ->
                1;
            _ ->
                I0 + 1
        end,
    Node = lists:nth(I, Nodes),
    lager:info("fuck I:~p, Node:~p", [I, Node]),
    Body = [{PHead0 + PLength, Node}|Body0],
    build_ring_body(Size, NodesLength, Nodes, PLength, N + 1, I, Body);
build_ring_body(Size, NodesLength, Nodes, PLength, _, _, []) ->
    Node = lists:nth(1, Nodes),
    Body = [{0, Node}],
    build_ring_body(Size, NodesLength, Nodes, PLength, 1, 1, Body).


locate(Ring, X) ->
    Hash = crypto:bytes_to_integer(crypto:hash(sha256, X)),
    #ring{plength = PLength, body = RingBody} = Ring,
    I = erlang:trunc(Hash / PLength),
    {V, _} = lists:nth(I + 1, RingBody),
    V.
