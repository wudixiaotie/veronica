%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica API
%% ===================================================================

-module(veronica).

%% APIs
-export([
         init_local_workers/1,
         get_worker/1,
         get_ring/0,
         local_partitions/1,
         join_cluster/1
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

init_local_workers(Module) ->
    case ?MODULE:get_ring() of
        {ok, Ring} ->
            lager:info("[veronica][worker] Initializing"),
            LPs = ?MODULE:local_partitions(Ring),
            init_local_workers(LPs, Module);
        not_found ->
            lager:debug("[veronica][worker] Waiting ring init"),
            timer:sleep(1000),
            init_local_workers(Module)
    end.

get_worker(Key) ->
    chash:get_worker(Key).

get_ring() ->
    ring:get().

local_partitions(Ring) ->
    ring:local_partitions(Ring).

join_cluster(Node) ->
    veronica_cluster:join(Node).


%%====================================================================
%% Internal functions
%%====================================================================

init_local_workers([LP|T], Module) ->
    PIndex = ring:partition_index(LP),
    veronica_worker:start(PIndex, Module),
    init_local_workers(T, Module);
init_local_workers([], _) ->
    ok.
