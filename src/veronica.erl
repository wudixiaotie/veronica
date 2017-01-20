%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica API
%% ===================================================================

-module(veronica).

%% APIs
-export([
         get_worker/1,
         get_ring/0,
         join_cluster/1
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

get_worker(Key) ->
    chash:get_worker(Key).

get_ring() ->
    ring:get().

join_cluster(Node) ->
    veronica_cluster:join(Node).


%%====================================================================
%% Internal functions
%%====================================================================
