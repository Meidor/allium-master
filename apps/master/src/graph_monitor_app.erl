%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 7:49 PM
%%%-------------------------------------------------------------------
-module(graph_monitor_app).
-behaviour(gen_server).

-define(INTERVAL, 5000).

%% API
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-spec start_link() -> any().
start_link() ->
    gen_server:start_link({local, graph_monitor_app}, ?MODULE, [], []).

-spec init(list()) -> tuple().
init([])->
    Timer = erlang:send_after(1, self(), check),
    {ok, Timer}.

-spec handle_info(atom(), any()) -> tuple().
handle_info(check, OldTimer) ->
    erlang:cancel_timer(OldTimer),
    node_graph_manager:rebuild_graph(),
    Timer = erlang:send_after(?INTERVAL, self(), check),
    {noreply, Timer}.

-spec handle_call(any(), any(), any()) -> any().
handle_call(_Request, _From, State) ->
    State.

-spec handle_cast(any(), any()) -> any().
handle_cast(_Request, State) ->
    State.

-spec terminate(any(), any()) -> any().
terminate(_Reason, State) ->
    State.

-spec code_change(any(), any(), any()) -> any().
code_change(_OldVsn, State, _Extra) ->
    State.