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
         join/1,
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

join(Node) ->
    pong = net_adm:ping(Node),
    send(rebalance).

members() ->
    ldb:ets_get(cluster, members).

add_member(Node) ->
    ?MODULE ! {add_member, Node}.

delete_member(Node) ->
    ?MODULE ! {delete_member, Node}.



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) ->
    ets:new(cluster, [set,
                      public,
                      named_table,
                      {read_concurrency, true}]),
    ldb:ets_set(cluster, members, [node()]),
    State = #state{refs = {node(), erlang:make_ref()}},
    {ok, State}.

handle_msg({add_member, Node}, 
           State = #state{refs = Refs}) ->
    {ok, Members} = ?MODULE:members(),
    Members1 = lists:sort([Node|Members]),
    ldb:ets_set(cluster, members, Members1),

    Ref = erlang:monitor(process, {?MODULE, Node}),
    {ok, State#state{refs = [{Node, Ref}|Refs]}};
handle_msg({delete_member, Node},
           State = #state{refs = Refs}) ->
    {ok, Members0} = ?MODULE:members(),
    Members = lists:sort(lists:delete(Node, Members0)),
    ldb:ets_set(cluster, members, Members),

    {value, {_, Ref}, Refs1} = lists:keytake(Node, 1, Refs),
    erlang:demonitor(Ref),
    {ok, State#state{refs = Refs1}};
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
    Members = ?MODULE:members(),
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
