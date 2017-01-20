%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica Cluster
%% ===================================================================

-module(veronica_cluster).

%% APIs
-export([
         join/1
        ]).



%% ===================================================================
%% API functions
%% ===================================================================

join(Node) ->
    pong = net_adm:ping(Node),
    {ok, Ring} = ring:build(),
    ring:store(Ring).
