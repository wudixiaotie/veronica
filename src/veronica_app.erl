%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica public API
%% ===================================================================

-module(veronica_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = veronica_sup:start_link(),
    ok = ring:init(),
    ok = init_local_workers(),
    {ok, SupPid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init_local_workers() ->
    lager:info("[veronica][worker] Initializing"),
    {ok, Ring} = ring:get(),
    LPs = ring:local_partitions(Ring),
    init_local_workers(LPs).

init_local_workers([LP|T]) ->
    PIndex = ring:partition_index(LP),
    veronica_worker:start(PIndex),
    init_local_workers(T);
init_local_workers([]) ->
    ok.
