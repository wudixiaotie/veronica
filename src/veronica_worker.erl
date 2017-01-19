%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica worker
%% ===================================================================

-module(veronica_worker).

-behaviour(gen_msg).

-export([
         start/1,
         start_link/1
        ]).

%% gen_msg callback functions
-export([
         init/1,
         handle_msg/2,
         terminate/2
        ]).

-record(state, {
          partition_index
         }).

-include("veronica.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

start(PIndex) ->
    supervisor:start_child(veronica_worker_sup, [PIndex]).

start_link(PIndex) ->
    gen_msg:start_link({global, ?WORKER_NAME(PIndex)}, ?MODULE, [PIndex], []).


%%====================================================================
%% gen_msg callback functions
%%====================================================================

init([PIndex]) ->
    {ok, #state{partition_index = PIndex}}.

handle_msg(Msg, State) ->
    lager:warning("[veronica][worker] Unknow msg ~p", [Msg]),
    {ok, State}.

terminate(Reason, _State) ->
    lager:error("[veronica][worker] Terminate ~p", [Reason]),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
