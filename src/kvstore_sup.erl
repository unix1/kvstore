-module(kvstore_sup).
-export([start_link/0]).
-export([init/1, start_server_sup/0, start_server/1, stop_server/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% wait for tables
init([]) ->
    MaxRestart = 1,
    MaxTime = 1,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_server_sup() ->
    ChildSpec = {serv_sup,
                 {kvstore_server_sup, start_link, []},
                  permanent, 10500, supervisor, [kvstore_server_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

start_server(Name) ->
    kvstore_server_sup:start_server(Name).

stop_server(Name) ->
    % TODO move these to kvstore_server_sup
    supervisor:terminate_child(kvstore_server_sup, Name),
    supervisor:delete_child(kvstore_server_sup, Name).