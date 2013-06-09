-module(kvstore).
-include("kvstore.hrl").
-export([install/1, install_all_nodes/0, start/2, stop/1]).
-export([start_server/0, start_server/1, stop_server/1]).
-export([
    read/1, read/2, read/3,
    write/2, write/3, write/4,
    delete/1, delete/2, delete/3,
    delete_match/1, delete_match/2, delete_match/3,
    delete_match_spec/1, delete_match_spec/2, delete_match_spec/3
]).
-behaviour(application).

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

install_all_nodes() ->
    install([node()|nodes()]).

start(normal, _Args) ->
    mnesia:wait_for_tables([kvstore_record], 5000),
    kvstore_sup:start_link(),
    kvstore_sup:start_server_sup().

stop(_) ->
    % instruct kvstore_sup to stop all children here
    ok.

%%%%% User API %%%%%

start_server() ->
    kvstore_sup:start_server().

start_server(Name) when is_atom(Name) ->
    % TODO check Name matches kv_xxx
    kvstore_sup:start_server(Name).

stop_server(Name) ->
    kvstore_sup:stop_server(Name).

read(Key) ->
    read(Key, transaction).

read(Key, {via, Name}) ->
    read(Key, {via, Name}, transaction);

read(Key, AccessContext) ->
    {ok, Pid, Name} = start_server(),
    Result = read(Key, {via, Pid}, AccessContext),
    stop_server(Name),
    Result.

read(Key, {via, Name}, AccessContext) ->
    kvstore_server:read(Name, Key, AccessContext).

write(Key, Value) ->
    write(Key, Value, transaction).

write(Key, Value, {via, Name}) ->
    write(Key, Value, {via, Name}, transaction);

write(Key, Value, AccessContext) ->
    {ok, Pid, Name} = start_server(),
    Result = write(Key, Value, {via, Pid}, AccessContext),
    stop_server(Name),
    Result.

write(Key, Value, {via, Name}, AccessContext) ->
    kvstore_server:write(Name, Key, Value, AccessContext).

delete(Key) ->
    delete(Key, transaction).

delete(Key, {via, Name}) ->
    delete(Key, {via, Name}, transaction);

delete(Key, AccessContext) ->
    {ok, Pid, Name} = start_server(),
    Result = delete(Key, {via, Pid}, AccessContext),
    stop_server(Name),
    Result.

delete(Key, {via, Name}, AccessContext) ->
    kvstore_server:delete(Name, Key, AccessContext).

delete_match(Pattern) ->
    delete_match(Pattern, transaction).

delete_match(Pattern, {via, Name}) ->
    delete_match(Pattern, {via, Name}, transaction);

delete_match(Pattern, AccessContext) ->
    {ok, Pid, Name} = start_server(),
    Result = delete_match(Pattern, {via, Pid}, AccessContext),
    stop_server(Name),
    Result.

delete_match(Pattern, {via, Name}, AccessContext) ->
    kvstore_server:delete_match(Name, Pattern, AccessContext).

delete_match_spec(Match) ->
    delete_match_spec(Match, transaction).

delete_match_spec(Match, {via, Name}) ->
    delete_match_spec(Match, {via, Name}, transaction);

delete_match_spec(Match, AccessContext) ->
    {ok, Pid, Name} = start_server(),
    Result = delete_match_spec(Match, {via, Pid}, AccessContext),
    stop_server(Name),
    Result.

delete_match_spec(Match, {via, Name}, AccessContext) ->
    kvstore_server:delete_match_spec(Name, Match, AccessContext).
