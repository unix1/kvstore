-module(kvstore_sup).
-export([start_link/0]).
-export([init/1]).
-behavior(supervisor).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% wait for tables
init([]) ->
    {ok, {{one_for_one, 1, 1}, []}}.