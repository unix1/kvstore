-module(kvstore).
-include("kvstore.hrl").
-export([install/1, start/2, stop/1]).
-export([read/1, read/2, write/2, write/3, delete/1, delete/2]).
-export([delete_match/1, delete_match/2]).
-behavior(application).

%%%%% Admin API %%%%%
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(kvstore_record,
        [
            {attributes, record_info(fields, kvstore_record)},
            {index, [#kvstore_record.time_accessed]},
            {disc_copies, Nodes},
            {type, set}
        ]
    ),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start(normal, []) ->
    mnesia:wait_for_tables([kvstore_record], 5000),
    kvstore_sup:start_link().

stop(_) ->
    ok.

%%%%% User API %%%%%

%% reads by Key
read(Key) ->
    read(Key, transaction).

%% reads by Key
%% allows to specify Mnesia access context
read(Key, AccessContext) ->
    case read_raw(Key, AccessContext) of
        [#kvstore_record{time_created = TC, time_modified = TM, time_accessed = _TA, value = V}] ->
            NewTA = now(),
            write_raw(
                #kvstore_record {
                    key = Key,
                    time_created = TC,
                    time_modified = TM,
                    time_accessed = NewTA,
                    value = V
                },
                AccessContext
            ),
            {Key, V, TC, TM, NewTA};
        [] ->
            undefined
    end.

%% writes by key
write(Key, Value) ->
    write(Key, Value, transaction).

%% writes by key
%% allows to specify Mnesia access context
write(Key, Value, AccessContext) ->
    Now = now(),
    % time_created will stay the same if modifying existing record
    ExistingRecord = read_raw(Key, AccessContext),
    %ExistingRecord = mnesia:read({kvstore_record, Key}),
    case ExistingRecord =:= [] of
        true ->
            TimeCreated = Now;
        false ->
            TimeCreated = ExistingRecord#kvstore_record.time_created
    end,
    write_raw(
        #kvstore_record {
            key = Key,
            time_created = TimeCreated,
            time_modified = Now,
            time_accessed = Now,
            value = Value
        },
        AccessContext
    ).

%% deletes by key
delete(Key) ->
    delete(Key, transaction).

%% deletes by key
%% allows to specify Mnesia access context
delete(Key, AccessContext) ->
    delete_raw(Key, AccessContext).

%% deletes by match spec
delete_match(Match) ->
    delete_match(Match, transaction).

%% deletes by match spec
%% allows to specify Mnesia access context
delete_match(Match, AccessContext) ->
    delete_match_raw(Match, AccessContext).

%%%%% Private functions %%%%%

%% wrapper around mnesia:read
read_raw(Key, AccessContext) ->
    F = fun() ->
        mnesia:read({kvstore_record, Key})
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around mnesia:write
write_raw(Record, AccessContext) ->
    F = fun() ->
        mnesia:write(Record)
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around mnesia:delete
delete_raw(Key, AccessContext) ->
    F = fun() -> 
        mnesia:delete({kvstore_record, Key})
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around match_object and delete_object sequence
delete_match_raw(Match, AccessContext) ->
    F = fun() ->
        ListToDelete = mnesia:match_object(Match),
        lists:foreach(
            fun(X) ->
                mnesia:delete_object(X)
            end,
            ListToDelete
        )
    end,
    mnesia:activity(AccessContext, F).