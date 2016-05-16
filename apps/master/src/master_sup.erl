%%%===================================================================
%% @doc master top level supervisor.
%% @end
%%%===================================================================

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(list()) -> tuple().
init([]) ->
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
        }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
