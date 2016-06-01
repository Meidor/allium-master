-module(master_sup).

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
    supervisor:start_link({local, master}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(list()) -> tuple().
init([]) ->
    init_shell().

-spec init_shell() -> tuple().
init_shell() ->
    {ok, { {one_for_one, 0, 1},
            []
        }
    }.

-spec init_full() -> tuple().
init_full() ->
    {ok, { {one_for_one, 0, 1},
        [
            {
                heartbeat_monitor_sup,
                {heartbeat_monitor_sup, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [heartbeat_monitor_sup]
            },
            {
                graph_monitor_sup,
                {graph_monitor_sup, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [graph_monitor_sup]
            }
        ]
    }}.