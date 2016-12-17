%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Consistent Hash
%% ===================================================================

-module(chash).

%% API
-export([
         get_worker/1
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

get_worker(Key) ->
    Hash = crypto:bytes_to_integer(
             crypto:hash(sha256, Key)),
    {ok, Ring} = ring:get(),
    Interval = ring:interval(Ring),
    I = erlang:trunc(Hash / Interval) + 1,
    Partitions = ring:partitions(Ring),
    Partition = lists:nth(I, Partitions),
    PIndex = ring:partition_index(Partition),
    {ok, ?WORKER_NAME(PIndex)}.
