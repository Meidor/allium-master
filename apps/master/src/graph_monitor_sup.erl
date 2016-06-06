-module(graph_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> any().
start_link() ->
    supervisor:start_link({local, graph_monitor}, ?MODULE, []).

-spec init(list()) -> tuple().
init([]) ->
    {ok, {
            {one_for_one, 0, 1},
            [{
                graph_monitor_app,
                {graph_monitor_app, start_link, []},
                permanent,
                brutal_kill,
                worker,
                [graph_monitor_app]
            }]
        }
    }.