-module(kvstore_sup).
-export([start_link/0]).
-export([init/1, start_server/1, stop_server/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, kvstore}, ?MODULE, []).

%% wait for tables
init([]) ->
    MaxRestart = 1,
    MaxTime = 1,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_server(Name) ->
    ChildSpec = {Name,
                 {kvstore_server_sup, start_link, [Name]},
                  permanent, 10500, supervisor, [kvstore_server_sup]},
    supervisor:start_child(kvstore, ChildSpec).

stop_server(Name) ->
    supervisor:terminate_child(kvstore, Name),
    supervisor:delete_child(kvstore, Name).