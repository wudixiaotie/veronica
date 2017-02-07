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
          name,
          cb_module, % callback module
          cb_state,   % callback state
          tranfers = <<>>
         }).



%% ===================================================================
%% behaviour callbacks
%% ===================================================================

-callback init(PIndex :: integer(), Args :: list()) ->
    {ok, State :: term()} |
    {stop, Reason :: term(), State :: term()}.
-callback transfer(State :: term()) -> {ok, Data :: term()}.
-callback finish_transfer(State :: term()) -> ok.
-callback receive_transfers(Data :: term(), State :: term()) -> {ok, NewState :: term()}.
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
    State = #state{name = ?VERONICA_WORKER(PIndex),
                   cb_module = CbModule},
    case CbModule:init(PIndex, Args) of
        {ok, CbState} ->
            {ok, State#state{cb_state = CbState}};
        {stop, Reason, CbState} ->
            {stop, Reason, State#state{cb_state = CbState}}
    end.

handle_msg({transfer, Member},
           State = #state{name = Name,
                          cb_module = CbModule,
                          cb_state = CbState}) ->
    lager:info("[veronica][worker ~p] Transferring to node: ~s",
               [Name, Member]),
    {ok, Data} = CbModule:transfer(CbState),
    ok = transfer_data({Name, Member}, Data),
    ok = CbModule:finish_transfer(CbState),
    {ok, State};
handle_msg({receive_transfers, From, Ref, DataBin},
           State = #state{name = Name,
                          tranfers = Tranfers}) ->
    lager:info("[veronica][worker ~p] Receiving transfer data",
               [Name]),
    Tranfers1 = <<Tranfers/binary, DataBin/binary>>,
    From ! {ack, Ref},
    {ok, State#state{tranfers = Tranfers1}};
handle_msg({finish_transfer, From, Ref},
           State = #state{name = Name,
                          cb_module = CbModule,
                          cb_state = CbState,
                          tranfers = Tranfers}) ->
    lager:info("[veronica][worker ~p] Finish transfer data",
               [Name]),
    Data = erlang:binary_to_term(Tranfers),
    {ok, NewCbState} = CbModule:receive_transfers(Data, CbState),
    From ! {ack, Ref},
    {ok, State#state{cb_state = NewCbState,
                     tranfers = <<>>}};
handle_msg(Msg, State = #state{name = Name}) ->
    lager:warning("[veronica][worker ~p] Unknow msg ~p",
                  [Name, Msg]),
    {ok, State}.

terminate({shutdown, transfered}, #state{name = Name}) ->
    lager:info("[veronica][worker ~p] transfered",
               [Name]),
    ok;
terminate(Reason, #state{name = Name,
                         cb_module = CbModule,
                         cb_state = CbState}) ->
    lager:error("[veronica][worker ~p] Terminate ~p",
                [Name, Reason]),
    CbModule:terminate(Reason, CbState),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

transfer_data(RemoteName, Data) ->
    DataBin = erlang:term_to_binary(Data),
    ok = do_transfer_data(RemoteName, DataBin),
    Ref = erlang:make_ref(),
    RemoteName ! {finish_transfer, self(), Ref},
    receive
        {ack, Ref} ->
            ok
    after
        5000 ->
            timeout
    end.

do_transfer_data(RemoteName, DataBin) when
      byte_size(DataBin) > 1024 ->
    Ref = erlang:make_ref(),
    <<D:1024/binary, T/binary>> = DataBin,
    RemoteName ! {receive_transfers, self(), Ref, D},
    receive
        {ack, Ref} ->
            do_transfer_data(RemoteName, T)
    after
        5000 ->
            timeout
    end;
do_transfer_data(RemoteName, DataBin) ->
    Ref = erlang:make_ref(),
    RemoteName ! {receive_transfers, self(), Ref, DataBin},
    receive
        {ack, Ref} ->
            ok
    after
        5000 ->
            timeout
    end.
