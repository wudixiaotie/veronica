%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica Cluster
%% ===================================================================

-module(veronica_cluster).

-behaviour(gen_msg).

%% APIs
-export([
         start_link/0,
         members/0,
         add_member/1,
         delete_member/1,
         active_members/0
        ]).

%% gen_msg callbacks
-export([
         init/1,
         handle_msg/2,
         terminate/2
        ]).

-include("veronica.hrl").

-record(state, {refs = []}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [], []).

members() ->
    ldb:ets_get(cluster, members).

add_member(Node) ->
    pong = net_adm:ping(Node),
    {?MODULE, Node} ! {add_member, node()}.

delete_member(Node) ->
    ?MODULE ! {delete_member, Node}.

active_members() ->
    ldb:ets_get(cluster, active_members).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) ->
    ets:new(cluster, [set,
                      public,
                      named_table,
                      {read_concurrency, true}]),
    ldb:ets_set(cluster, members, [node()]),
    ldb:ets_set(cluster, active_members, [node()]),
    State = #state{refs = [{node(), erlang:make_ref()}]},
    {ok, State}.

handle_msg({add_member, Node},
           State = #state{refs = Refs}) ->
    {ok, Members} = ?MODULE:members(),
    Members1 = lists:sort([Node|Members]),
    ldb:ets_set(cluster, members, Members1),
    {?MODULE, Node} ! {members, Members1},

    Ref = erlang:monitor(process, {?MODULE, Node}),
    {ok, AMembers} = ?MODULE:active_members(),
    AMembers1 = lists:sort([Node|AMembers]),
    ldb:ets_set(cluster, active_members, AMembers1),

    send(rebalance),
    {ok, State#state{refs = [{Node, Ref}|Refs]}};
handle_msg({members, Members},
           State = #state{refs = Refs}) ->
    lager:info("[veronica][cluster] Update members"),
    ldb:ets_set(cluster, members, Members),
    ldb:ets_set(cluster, active_members, Members),
    Refs1 =
        lists:foldl(
            fun(Node, Acc) ->
                case lists:keytake(Node, 1, Acc) of
                    {value, {_, Ref}, Acc1} ->
                        erlang:demonitor(Ref);
                    false ->
                        Acc1 = Acc
                end,
                Ref1 = erlang:monitor(process, {?MODULE, Node}),
                [{Node, Ref1}|Acc1]
            end, Refs, Members),
    {ok, State#state{refs = Refs1}};
handle_msg({delete_member, Node},
           State = #state{refs = Refs}) ->
    {ok, Members} = ?MODULE:members(),
    Members1 = lists:sort(lists:delete(Node, Members)),
    ldb:ets_set(cluster, members, Members1),

    {value, {_, Ref}, Refs1} = lists:keytake(Node, 1, Refs),
    erlang:demonitor(Ref),
    node_down(Node),

    send(rebalance),
    {ok, State#state{refs = Refs1}};
handle_msg({'DOWN', Ref, process, _Object, Info},
           State = #state{refs = Refs}) ->
    case lists:keytake(Ref, 2, Refs) of
        {value, {Node, _}, Refs1} ->
            lager:info("[veronica][cluster] ~p has DOWN, reason: ~p",
                       [Node, Info]),
            node_down(Node),
            {ok, State#state{refs = Refs1}};
        false ->
            {ok, State}
    end;
handle_msg(rebalance, State) ->
    lager:info("[veronica][cluster] Rebalancing"),
    {ok, Ring} = ring:build(),
    ok = ring:store(Ring),
    Partitions = ring:partitions(Ring),
    ok = rebalance(Partitions),
    {ok, State};
handle_msg(_Msg, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

send(Msg) ->
    {ok, Members} = ?MODULE:members(),
    send(Members, Msg).

send([Node|Nodes], Msg) ->
    {?MODULE, Node} ! Msg,
    send(Nodes, Msg);
send([], _Msg) ->
    ok.

rebalance([#partition{member = Member}|Partitions]) when
      Member == node() ->
    rebalance(Partitions);
rebalance([#partition{index = PIndex, member = Member}
           |Partitions]) ->
    ?VERONICA_WORKER(PIndex) ! {transfer, Member},
    rebalance(Partitions);
rebalance([]) ->
    ok.

node_down(Node) ->
    {ok, AMembers} = ?MODULE:active_members(),
    AMembers1 = lists:sort(lists:delete(Node, AMembers)),
    ldb:ets_set(cluster, active_members, AMembers1),
    ok.