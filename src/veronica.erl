%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica API
%% ===================================================================

-module(veronica).

%% APIs
-export([
         init_local_workers/2,
         get_worker/1,
         get_ring/0,
         local_partitions/1,
         join_cluster/1,
         members/0,
         active_members/0
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

init_local_workers(Module, Args) ->
    case ?MODULE:get_ring() of
        {ok, Ring} ->
            lager:info("[veronica][worker] Initializing"),
            LPs = ?MODULE:local_partitions(Ring),
            init_local_workers(LPs, Module, Args);
        not_found ->
            lager:debug("[veronica][worker] Waiting ring init"),
            timer:sleep(1000),
            init_local_workers(Module, Args)
    end.

get_worker(Key) ->
    {ok, Partition} = chash:hash(Key),
    Member = ring:partition_member(Partition),
    Index = ring:partition_index(Partition),
    Worker = {?VERONICA_WORKER(Index), Member},
    {ok, Worker}.

get_ring() ->
    ring:get().

local_partitions(Ring) ->
    ring:local_partitions(Ring).

join_cluster(Node) ->
    veronica_cluster:add_member(Node).

members() ->
    veronica_cluster:members().

active_members() ->
    veronica_cluster:active_members().


%%====================================================================
%% Internal functions
%%====================================================================

init_local_workers([LP|T], Module, Args) ->
    PIndex = ring:partition_index(LP),
    veronica_worker:start(PIndex, Module, Args),
    init_local_workers(T, Module, Args);
init_local_workers([], _, _) ->
    ok.
