%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Ring
%% ===================================================================

-module(ring).

%% API
-export([
         init/0,
         build/0,
         store/1,
         get/0,
         interval/1,
         partitions/1,
         partition_index/1,
         lower_bound/1,
         local_partitions/1
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

init() ->
    lager:info("[veronica][ring] Initializing"),
    ets:new(ring, [set,
                   public,
                   named_table,
                   {read_concurrency, true}]),
    {ok, Ring} = build(),
    store(Ring),
    ok.

build() ->
    Size = 64,
    Max = math:pow(2, 256),
    Interval = erlang:trunc(Max / Size),
    Members = veronica_cluster:members(),
    Ring0 = #ring{size = Size,
                  interval = Interval,
                  members = Members},
    {ok, Ring} = build_partitions(Ring0),
    {ok, Ring}.

store(Ring = #ring{}) ->
    RingBin = erlang:term_to_binary(Ring),
    ok = ldb:set(<<"ring">>, RingBin),
    true = ets:insert(ring, {ring, Ring}),
    ok.

get() ->
    case ets:lookup(ring, ring) of
        [{ring, Ring}] ->
            {ok, Ring};
        _ ->
            {ok, RingBin} = ldb:get(<<"ring">>),
            Ring = erlang:binary_to_term(RingBin),
            true = ets:insert(ring, {ring, Ring}),
            {ok, Ring}
    end.

interval(#ring{interval = Interval}) ->
    Interval.

partitions(#ring{partitions = Partitions}) ->
    Partitions.

partition_index(#partition{index = Index}) ->
    Index.

lower_bound(#partition{lower_bound = LowerBound}) ->
    LowerBound.

local_partitions(#ring{partitions = Partitions}) ->
    local_partitions(node(), Partitions, []).



%% ===================================================================
%% Internal functions
%% ===================================================================

build_partitions(#ring{size = Size,
                       interval = Interval,
                       members = Members} = Ring) ->
    MembersCount = erlang:length(Members),
    {ok, Partitions} = build_partitions(Size,
                                        MembersCount,
                                        Members,
                                        Interval,
                                        0, 0, []),
    {ok, Ring#ring{partitions = Partitions}}.

build_partitions(Size, _, _, _, Size, _, Body) ->
    {ok, lists:reverse(Body)};
build_partitions(Size, MembersCount, Members, Interval, N, I0,
                 Partitions0 =
                     [#partition{index = Index0,
                                 lower_bound = LowerBound0}|_]) ->
    I = case I0 of
            MembersCount ->
                1;
            _ ->
                I0 + 1
        end,
    Member = lists:nth(I, Members),
    Partition = #partition{index = Index0 + 1,
                           lower_bound = LowerBound0 + Interval,
                           member = Member},
    Partitions = [Partition|Partitions0],
    build_partitions(Size, MembersCount, Members,
                     Interval, N + 1, I, Partitions);
build_partitions(Size, MembersCount, Members,
                 Interval, _, _, []) ->
    Member = lists:nth(1, Members),
    Partition = #partition{index = 1,
                           lower_bound = 0,
                           member = Member},
    Partitions = [Partition],
    build_partitions(Size, MembersCount, Members,
                     Interval, 1, 1, Partitions).

local_partitions(Self, [#partition{member = Self} = P|T], LPs) ->
    local_partitions(Self, T, [P|LPs]);
local_partitions(Self, [_|T], LPs) ->
    local_partitions(Self, T, LPs);
local_partitions(_, [], LPs) ->
    LPs.

