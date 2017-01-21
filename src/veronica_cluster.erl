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
         members/0
        ]).

%% gen_msg callbacks
-export([
         init/1,
         handle_msg/2,
         terminate/2
        ]).

-include("veronica.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Node) ->
    pong = net_adm:ping(Node),
    send(rebalance).

members() ->
    Nodes = [erlang:node()|erlang:nodes()],
    lists:sort(Nodes).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) -> {ok, []}.

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
