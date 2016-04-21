%%%-------------------------------------------------------------------
%%% @author Niels & Koen
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 7:49 PM
%%%-------------------------------------------------------------------
-module(heartbeat_monitor_app).
-behaviour(gen_server).

-define(INTERVAL, 5000).

%% API
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, heartbeat_monitor_app}, ?MODULE, [], []).

init([])->
  Timer = erlang:send_after(1, self(), check),
  {ok, Timer}.

handle_info(check, OldTimer) ->
  erlang:cancel_timer(OldTimer),
  heartbeat_monitor:remove_inactive_nodes(?INTERVAL),
  Timer = erlang:send_after(?INTERVAL, self(), check),
  {noreply, Timer}.

handle_call(_Request, _From, State) ->
  State.

handle_cast(_Request, State) ->
  State.

terminate(_Reason, State) ->
  State.

code_change(_OldVsn, State, _Extra) ->
  State.