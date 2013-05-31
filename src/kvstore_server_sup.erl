-module(kvstore_server_sup).
-export([start_link/0, init/1, start_server/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_server(Name) ->
    ChildSpec = {
        Name,
        {kvstore_server, start_link, [Name, self()]},
        permanent,
        5000, % shutdown time
        worker,
        [kvstore_server]
    },
    supervisor:start_child(?MODULE, ChildSpec).