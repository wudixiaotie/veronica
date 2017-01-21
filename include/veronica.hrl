%% ===================================================================
%% Author Kevin Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica Header file
%% ===================================================================

-define(VERONICA_WORKER(PIndex),
        erlang:list_to_atom("veronica_worker_" ++ erlang:integer_to_list(PIndex))).

-record(ring, {size,
               interval, %% partition interval
               members,
               partitions}).

-record(partition, {index,
                    lower_bound,
                    member}).
