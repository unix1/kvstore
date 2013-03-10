-module(kvstore_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([write/1, read/1, delete/1]).

all() -> [write, read, delete].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    kvstore:install([node()]),
    application:start(mnesia),
    application:start(kvstore),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

% write KV pair
write(_Config) ->
    ok = kvstore:write(
        "key-1234",
        [{test, "record value"}]
    ).

read(_Config) ->
    TestKey = "key-for-testing-read",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue),
    {TestKey, TestValue, _, _, _} = kvstore:read(TestKey),
    undefined = kvstore:read("some-random-non-existent-key").

delete(_Config) ->
    TestKey = "key-for-testing-delete",
    TestValue = [
        {username, "some-test-username"},
        {session_value, "some value for testing"},
        user
    ],
    ok = kvstore:write(TestKey, TestValue),
    ok = kvstore:delete(TestKey),
    undefined = kvstore:read(TestKey).