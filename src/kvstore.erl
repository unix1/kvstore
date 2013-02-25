-module(kvstore).
-export([install/1, start/2, stop/1]).
-export([read/1, read/2, write/2, write/3]).
-behavior(application).

%% record definition
-record (
    kvstore_record,
    {
        key,
        time_created,
        time_modified,
        time_accessed,
        value
    }
).

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
    case read_quiet(Key, AccessContext) of
        [#kvstore_record{time_created = TC, time_modified = TM, time_accessed = _TA, value = V}] ->
            NewTA = now(),
            write_quiet(
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
    ExistingRecord = read_quiet(Key, AccessContext),
    %ExistingRecord = mnesia:read({kvstore_record, Key}),
    case ExistingRecord =:= [] of
        true ->
            TimeCreated = Now;
        false ->
            TimeCreated = ExistingRecord#kvstore_record.time_created
    end,
    write_quiet(
        #kvstore_record {
            key = Key,
            time_created = TimeCreated,
            time_modified = Now,
            time_accessed = Now,
            value = Value
        },
        AccessContext
    ).

%%%%% private functions %%%%%

%% wrapper around mnesia:read
read_quiet(Key, AccessContext) ->
    F = fun() ->
        mnesia:read({kvstore_record, Key})
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around mnesia:write
write_quiet(Record, AccessContext) ->
    F = fun() ->
        mnesia:write(Record)
    end,
    mnesia:activity(AccessContext, F).