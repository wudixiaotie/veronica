-module(tv).

-behaviour(veronica_worker).

-export([start/0]).

-export([init/0, transfer/2, terminate/2]).

start() ->
    application:start(veronica),
    veronica:init_local_workers(?MODULE).

init() ->
    lager:info("[tv] init"),
    {ok, []}.

transfer(Member, _State) ->
    lager:info("[tv] transfer ~p", [Member]),
    ok.

terminate(_Reason, _State) ->
    lager:error("[tv] terminate"),
    ok.
