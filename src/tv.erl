-module(tv).

-behaviour(veronica_worker).

-export([start/0, join/1]).

-export([init/2, transfer/1, receive_transfers/2, finish_transfer/1, terminate/2]).

start() ->
    application:start(veronica),
    veronica:init_local_workers(?MODULE, []).

join(Node) ->
    veronica:join_cluster(Node).

init(PIndex, _Args) ->
    lager:info("[tv] init ~p", [PIndex]),
    {ok, []}.

receive_transfers(Data, State) ->
    lager:info("[tv] receive transfer data ~p", [Data]),
    {ok, State}.

finish_transfer(_state) ->
    lager:info("[tv] finish transfer data"),
    ok.

transfer(_State) ->
    lager:info("[tv] transfer"),
    {ok, "alksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjh"}.

terminate(_Reason, _State) ->
    lager:error("[tv] terminate"),
    ok.
