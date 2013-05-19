-module(kvstore_server_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).

start_link(Name) ->
    supervisor:start_link(?MODULE, {Name}).

init({Name}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime},
          [{serv,
             {kvstore_server, start_link, [Name, self()]},
             permanent,
             5000, % Shutdown time
             worker,
             [kvstore_server]}]}}.