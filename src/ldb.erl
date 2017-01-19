%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% local database
%% ===================================================================

-module(ldb).

-behaviour(gen_msg).

%% API
-export([
         start_link/0,
         get/1,
         set/2
        ]).

%% gen_msg callbacks
-export([
         init/1,
         handle_msg/2,
         terminate/2
        ]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) when
      is_binary(Key) ->
    case ets:lookup(ldb, handle) of
        [{handle, DBHandle}] ->
            erocksdb:get(DBHandle, Key,
                         [{verify_checksums, true}]);
        _ ->
            error
    end.

set(Key, Value) when
      is_binary(Key),
      is_binary(Value) ->
    case ets:lookup(ldb, handle) of
        [{handle, DBHandle}] ->
            erocksdb:put(DBHandle, Key, Value,
                         [{sync, true}]);
        _ ->
            error
    end.



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) ->
    DBPath = application:get_env(veronica, db_path, "data/"),
    {ok, DBHandle} = erocksdb:open(DBPath ++ "main",
                                   [{create_if_missing, true}], []),
    ets:new(ldb, [set,
                  public,
                  named_table,
                  {read_concurrency, true}]),
    true = ets:insert(ldb, {handle, DBHandle}),
    {ok, []}.

handle_msg(_Msg, State) -> {ok, State}.
terminate(_Reason, _State) -> ok.
