%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica top level supervisor.
%% ===================================================================

-module(veronica_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), #{id        => I,
                          start     => {I, start_link, []},
                          restart   => permanent,
                          type      => Type}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1},
           [?CHILD(ldb, worker),
            ?CHILD(veronica_worker_sup, supervisor)]} }.

%%====================================================================
%% Internal functions
%%====================================================================
