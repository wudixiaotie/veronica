%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica worker
%% ===================================================================

-module(veronica_worker).

-behaviour(gen_msg).

-export([
         start/3,
         start_link/3
        ]).

%% gen_msg callback functions
-export([
         init/1,
         handle_msg/2,
         terminate/2
        ]).

-include("veronica.hrl").

-record(state, {
          cb_module, % callback module
          cb_state   % callback state
         }).



%% ===================================================================
%% behaviour callbacks
%% ===================================================================

-callback init(Args :: list()) ->
    {ok, State :: term()} |
    {stop, Reason :: term(), State :: term()}.
-callback transfer(Member :: atom(), State :: term()) -> ok.
-callback terminate(Reason :: term(), State :: term()) -> ok.



%% ===================================================================
%% API functions
%% ===================================================================

start(PIndex, CbModule, Args) ->
    supervisor:start_child(veronica_worker_sup, [PIndex, CbModule, Args]).

start_link(PIndex, CbModule, Args) ->
    gen_msg:start_link({local, ?VERONICA_WORKER(PIndex)}, ?MODULE, [PIndex, CbModule, Args], []).

%%====================================================================
%% gen_msg callback functions
%%====================================================================

init([PIndex, CbModule, Args]) ->
    State = #state{cb_module = CbModule},
    case CbModule:init(PIndex, Args) of
        {ok, CbState} ->
            {ok, State#state{cb_state = CbState}};
        {stop, Reason, CbState} ->
            {stop, Reason, State#state{cb_state = CbState}}
    end.

handle_msg({transfer, Member},
           State = #state{cb_module = CbModule,
                          cb_state = CbState}) ->
    lager:info("[veronica][worker] Transferring to node: ~s",
               [Member]),
    ok = CbModule:transfer(Member, CbState),
    {stop, transfered, State};
handle_msg(Msg, State) ->
    lager:warning("[veronica][worker] Unknow msg ~p", [Msg]),
    {ok, State}.

terminate(Reason, #state{cb_module = CbModule,
                         cb_state = CbState}) ->
    lager:error("[veronica][worker] Terminate ~p", [Reason]),
    CbModule:terminate(Reason, CbState),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
