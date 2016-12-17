%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica API
%% ===================================================================

-module(veronica).

%% API
-export([
         init/0,
         error/1
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

init() ->
    ok = ldb:init(),
    ok = ring:init(),
    %%ok = init_local_workers(),
    ok.

error(X) ->
    lager:error("[text] ~p~n", [X]).



%%====================================================================
%% Internal functions
%%====================================================================

init_local_workers() ->
    {ok, Ring} = ring:get(),
    LPs = ring:local_partitions(Ring),
    init_local_workers(LPs).

init_local_workers([LP|T]) ->
    PIndex = ring:partition_index(LP),
    supervisor:start_child(veronica_worker_sup,
                           [PIndex]),
    init_local_workers(T);
init_local_workers([]) ->
    ok.
