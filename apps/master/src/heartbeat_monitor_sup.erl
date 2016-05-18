-module(heartbeat_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> any().
start_link() ->
    supervisor:start_link({local, heartbeat_monitor}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(list()) -> tuple().
init([]) ->
    {ok, {
            {one_for_one, 0, 1},
            [
                {
                    heartbeat_monitor_clients_app,
                    {heartbeat_monitor_clients_app, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [heartbeat_monitor_clients_app]
                },
                {
                    heartbeat_monitor_nodes_app,
                    {heartbeat_monitor_nodes_app, start_link, []},
                    permanent,
                    brutal_kill,
                    worker,
                    [heartbeat_monitor_nodes_app]
                }
            ]
        }
    }.