%% ===================================================================
%% Author Tie Xiao
%% Email wudixiaotie@gmail.com
%% 2016-11-24
%% local database
%% ===================================================================

-module(ldb).

%% API
-export([
         init/0,
         get/1,
         set/2
        ]).



%% ===================================================================
%% API functions
%% ===================================================================

init() ->
    {ok, DBHandle} = erocksdb:open("data/test.db",
                             [{create_if_missing, true}], []),
    ets:new(ldb, [set,
                  public,
                  named_table,
                  {read_concurrency, true}]),
    true = ets:insert(ldb, {handle, DBHandle}),
    ok.

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
