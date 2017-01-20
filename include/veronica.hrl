%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% Veronica Header file
%% ===================================================================

-define(WORKER_NAME(PIndex),
        erlang:list_to_atom("veronica_worker_" ++ erlang:integer_to_list(PIndex))).
