-module(kvstore_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/kvstore.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([write/1, read/1, writeover/1, delete/1, delete_match/1, delete_match_spec/1]).

all() -> [write, read, writeover, delete, delete_match, delete_match_spec].

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

%% test write key value pair
write(_Config) ->
    ok = kvstore:write(
        kvtest,
        "key-1234",
        [{test, "record value"}]
    ).

%% test read by key
read(_Config) ->
    TestKey = "key-for-testing-read",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(kvtest, TestKey, TestValue),
    {TestKey, TestValue, _, _, _} = kvstore:read(kvtest, TestKey),
    undefined = kvstore:read(kvtest, "some-random-non-existent-key").

%% test write over existing record by key
writeover(_Config) ->
    TestKey = "key1",
    TestValue1 = "value1",
    TestValue2 = "value2",
    ok = kvstore:write(kvtest, TestKey, TestValue1),
    ok = kvstore:write(kvtest, TestKey, TestValue2),
    {TestKey, TestValue2, _, _, _} = kvstore:read(kvtest, TestKey).

%% test delete by key
delete(_Config) ->
    TestKey = "key-for-testing-delete",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(kvtest, TestKey, TestValue),
    ok = kvstore:delete(kvtest, TestKey),
    undefined = kvstore:read(kvtest, TestKey).

%% test delete by match
delete_match(_Config) ->
    TestKey1 = "key1",
    TestValue1 = "value1",
    TestKey2 = "key2",
    TestValue2 = "value2",
    ok = kvstore:write(kvtest, TestKey1, TestValue1),
    ok = kvstore:write(kvtest, TestKey2, TestValue2),
    DeleteSpec = #kvstore_record{value = TestValue2, _ = '_'},
    ok = kvstore:delete_match(kvtest, DeleteSpec),
    {TestKey1, TestValue1, _, _, _} = kvstore:read(kvtest, TestKey1),
    undefined = kvstore:read(kvtest, TestKey2).

%% test delete by match spec
delete_match_spec(_Config) ->
    TestKey = "key-1234",
    ok = kvstore:write(
        kvtest,
        TestKey,
        [{test, "record value"}]
    ),
    DeleteSpec = [{
        #kvstore_record{key='$1', time_accessed='$2', _='_'},
        [{'<', '$2', {erlang:now()}}],
        ['$1']
    }],
    ok = kvstore:delete_match_spec(kvtest, DeleteSpec),
    undefined = kvstore:read(kvtest, TestKey).