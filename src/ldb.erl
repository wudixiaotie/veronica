%% ===================================================================
%% Author Kevin Xiao
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
         set/2,
         ets_get/2,
         ets_set/3
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

ets_get(Table, Key) when
      is_atom(Table),
      is_atom(Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Value}] ->
            {ok, Value};
        _ ->
            KeyBin = erlang:atom_to_binary(Key, utf8),
            case ?MODULE:get(KeyBin) of
                {ok, ValueBin} ->
                    Value = erlang:binary_to_term(ValueBin),
                    true = ets:insert(Key, {Key, Value}),
                    {ok, Value};
                not_found ->
                    not_found
            end
    end.

ets_set(Table, Key, Value) when
      is_atom(Table),
      is_atom(Key) ->
    KeyBin = erlang:atom_to_binary(Key, utf8),
    ValueBin = erlang:term_to_binary(Value),
    ok = ?MODULE:set(KeyBin, ValueBin),
    true = ets:insert(Table, {Key, Value}),
    ok.



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
