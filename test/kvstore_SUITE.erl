-module(kvstore_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/kvstore.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([
    start_server/1,
    write_via/1, read_via/1, writeover_via/1, delete_via/1, delete_match_via/1,
    delete_match_spec_via/1,
    write/1, read/1, writeover/1, delete/1, delete_match/1, delete_match_spec/1
]).

all() -> [
    start_server, write, write_via, read, read_via, writeover, writeover_via,
    delete, delete_via, delete_match, delete_match_via,
    delete_match_spec, delete_match_spec_via].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    kvstore:install([node()]),
    application:start(mnesia),
    application:start(kvstore),
    kvstore:start_server(kvtest),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% test starting and stopping a gen_server worker
start_server(_Config) ->
    {ok, _Pid} = kvstore:start_server(srv_start_server1),
    ok = kvstore:stop_server(srv_start_server1).

%% test write key value pair
write(_Config) ->
    ok = kvstore:write(
        "key-write",
        [{test, "record value 1"}]
    ).
write_via(_Config) ->
    ok = kvstore:write(
        "key-write-via",
        [{test, "record value 2"}],
        {via, kvtest}
    ).

%% test read by key
read(_Config) ->
    TestKey = "key-read",
    TestValue = [
        {username, "some-test-username1"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue),
    {TestKey, TestValue, _, _, _} = kvstore:read(TestKey),
    undefined = kvstore:read("some-random-non-existent-key").

read_via(_Config) ->
    TestKey = "key-read-via",
    TestValue = [
        {username, "some-test-username2"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue, {via, kvtest}),
    {TestKey, TestValue, _, _, _} = kvstore:read(TestKey, {via, kvtest}),
    undefined = kvstore:read("some-random-non-existent-key", {via, kvtest}).

%% test write over existing record by key
writeover(_Config) ->
    TestKey = "key-writeover-1",
    TestValue1 = "value1",
    TestValue2 = "value2",
    ok = kvstore:write(TestKey, TestValue1),
    ok = kvstore:write(TestKey, TestValue2),
    {TestKey, TestValue2, _, _, _} = kvstore:read(TestKey).

writeover_via(_Config) ->
    TestKey = "key-writeover-via-1",
    TestValue1 = "value1",
    TestValue2 = "value2",
    ok = kvstore:write(TestKey, TestValue1, {via, kvtest}),
    ok = kvstore:write(TestKey, TestValue2, {via, kvtest}),
    {TestKey, TestValue2, _, _, _} = kvstore:read(TestKey, {via, kvtest}).

%% test delete by key
delete(_Config) ->
    TestKey = "key-delete",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue),
    ok = kvstore:delete(TestKey),
    undefined = kvstore:read(TestKey).

delete_via(_Config) ->
    TestKey = "key-delete-via",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue, {via, kvtest}),
    ok = kvstore:delete(TestKey, {via, kvtest}),
    undefined = kvstore:read(TestKey, {via, kvtest}).

%% test delete by match
delete_match(_Config) ->
    TestKey1 = "key-delete-match1",
    TestValue1 = "value1",
    TestKey2 = "key-delete-match2",
    TestValue2 = "value2",
    ok = kvstore:write(TestKey1, TestValue1),
    ok = kvstore:write(TestKey2, TestValue2),
    DeleteSpec = #kvstore_record{value = TestValue2, _ = '_'},
    ok = kvstore:delete_match(DeleteSpec),
    {TestKey1, TestValue1, _, _, _} = kvstore:read(TestKey1),
    undefined = kvstore:read(TestKey2).

delete_match_via(_Config) ->
    TestKey1 = "key-delete-match-via1",
    TestValue1 = "value1",
    TestKey2 = "key-delete-match-via2",
    TestValue2 = "value2",
    ok = kvstore:write(TestKey1, TestValue1, {via, kvtest}),
    ok = kvstore:write(TestKey2, TestValue2, {via, kvtest}),
    DeleteSpec = #kvstore_record{value = TestValue2, _ = '_'},
    ok = kvstore:delete_match(DeleteSpec, {via, kvtest}),
    {TestKey1, TestValue1, _, _, _} = kvstore:read(TestKey1, {via, kvtest}),
    undefined = kvstore:read(TestKey2, {via, kvtest}).

%% test delete by match spec
delete_match_spec(_Config) ->
    TestKey = "key-delete-match-spec",
    ok = kvstore:write(TestKey, [{test, "record value"}]),
    DeleteSpec = [{
        #kvstore_record{key='$1', time_accessed='$2', _='_'},
        [{'<', '$2', {os:timestamp()}}],
        ['$1']
    }],
    ok = kvstore:delete_match_spec(DeleteSpec),
    undefined = kvstore:read(TestKey).

delete_match_spec_via(_Config) ->
    TestKey = "key-delete-match-spec-via",
    ok = kvstore:write(
        TestKey,
        [{test, "record value"}],
        {via, kvtest}
    ),
    DeleteSpec = [{
        #kvstore_record{key='$1', time_accessed='$2', _='_'},
        [{'<', '$2', {os:timestamp()}}],
        ['$1']
    }],
    ok = kvstore:delete_match_spec(DeleteSpec, {via, kvtest}),
    undefined = kvstore:read(TestKey, {via, kvtest}).
