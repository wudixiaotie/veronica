%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Consistent Hash
%% ===================================================================

-module(chash).

%% APIs
-export([
         hash/1
        ]).



%% ===================================================================
%% API functions
%% ===================================================================

hash(Key) ->
    Hash = crypto:bytes_to_integer(
             crypto:hash(sha256, Key)),
    {ok, Ring} = ring:get(),
    Interval = ring:interval(Ring),
    I = erlang:trunc(Hash / Interval) + 1,
    Partitions = ring:partitions(Ring),
    Partition = lists:nth(I, Partitions),
    {ok, Partition}.
