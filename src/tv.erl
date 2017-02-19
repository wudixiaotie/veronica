-module(tv).

-behaviour(veronica_worker).

-export([start/0, join/1, send/1, members/0, active_members/0]).

-export([init/2, handle_msg/2, transfer/1, receive_transfers/2, finish_transfer/1, terminate/2]).

-record(state, {name}).

-include("veronica.hrl").

start() ->
    application:start(veronica),
    veronica:init_local_workers(?MODULE, []).

send(Msg) ->
    {ok, Worker} = veronica:get_worker(Msg),
    Worker ! Msg.

join(Node) ->
    veronica:join_cluster(Node).

members() ->
    veronica:members().

active_members() ->
    veronica:active_members().

init(PIndex, _Args) ->
    Name = ?VERONICA_WORKER(PIndex),
    lager:info("[tv] init ~p", [Name]),
    {ok, #state{name=Name}}.

receive_transfers(Data, State) ->
    lager:info("[tv] receive transfer data ~p", [Data]),
    {ok, State}.

finish_transfer(_state) ->
    lager:info("[tv] finish transfer data"),
    ok.

transfer(_State) ->
    lager:info("[tv] transfer"),
    {ok, "alksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxbjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjhalksdjfaposdfaj d japosdjf;laksdjf paidjg;alwerj;alsdkcjzpoiicuvwerfladkjvzpxicohv qbelf alksdjfasudhflaksdj zncvpziuxycfqlkwejhrqpodivcyhlzkxjh"}.

terminate(_Reason, _State) ->
    lager:error("[tv] terminate"),
    ok.

handle_msg(Msg, State = #state{name = Name}) ->
    lager:info("[tv][~p] got msg ~p", [Name, Msg]),
    {ok, State}.
