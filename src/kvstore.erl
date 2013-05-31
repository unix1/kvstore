-module(kvstore).
-include("kvstore.hrl").
-export([install/1, install_all_nodes/0, start/2, stop/1]).
-export([start_server/1, stop_server/1]).
-export([read/2, read/3, write/3, write/4, delete/2, delete/3]).
-export([delete_match/2, delete_match/3, delete_match_spec/2, delete_match_spec/3]).
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

start_server(Name) ->
    kvstore_sup:start_server(Name).

stop_server(Name) ->
    kvstore_sup:stop_server(Name).

read(Name, Key) ->
    kvstore_server:read(Name, Key).

read(Name, Key, AccessContext) ->
    kvstore_server:read(Name, Key, AccessContext).

write(Name, Key, Value) ->
    kvstore_server:write(Name, Key, Value).

write(Name, Key, Value, AccessContext) ->
    kvstore_server:write(Name, Key, Value, AccessContext).

delete(Name, Key) ->
    kvstore_server:delete(Name, Key).

delete(Name, Key, AccessContext) ->
    kvstore_server:delete(Name, Key, AccessContext).

delete_match(Name, Pattern) ->
    kvstore_server:delete_match(Name, Pattern).

delete_match(Name, Pattern, AccessContext) ->
    kvstore_server:delete_match(Name, Pattern, AccessContext).

delete_match_spec(Name, Match) ->
    kvstore_server:delete_match_spec(Name, Match).

delete_match_spec(Name, Match, AccessContext) ->
    kvstore_server:delete_match_spec(Name, Match, AccessContext).
