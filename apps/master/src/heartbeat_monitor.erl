%%%-------------------------------------------------------------------
%%% @author Koen & Niels
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 10:57 AM
%%%-------------------------------------------------------------------
-module(heartbeat_monitor).
-author("Koen & Niels").

%% API
-export([receive_heartbeat/2,
  remove_inactive_nodes/1,
  add_node/1,
  remove_node/1,
  get_current_time/0]).

receive_heartbeat(NodeId, SecretHash) when is_list(NodeId), is_list(SecretHash) ->
  Node_verified = node_service:verify_node(NodeId, SecretHash),
  case Node_verified of
    ok ->
      redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()),
      ok;
    _ ->
      {error, "Node id and secret hash do not match"}
  end.

remove_inactive_nodes(TimeBetweenHeartbeats) when is_integer(TimeBetweenHeartbeats) ->
  AllKeys = [binary_to_list(Key) || Key <- redis:get_matching_keys("heartbeat_node_")],
  AllValues = [binary_to_integer(Value) || Value <- redis:get_list(AllKeys)],
  ExpiredNodes = [Key || {Key, Value} <- lists:zip(AllKeys, AllValues), Value < (?MODULE:get_current_time()- TimeBetweenHeartbeats)],
  RemovedNodes = [string:substr(Key, 22)  || Key <- ExpiredNodes],
  lists:foreach(fun(Node) -> node_service:node_unregister(Node), remove_node(Node) end, RemovedNodes),
  RemovedNodes.

add_node(NodeId) when is_list(NodeId) ->
  redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()),
  ok.

remove_node(NodeId) when is_list(NodeId) ->
  redis:remove("heartbeat_node_" ++ NodeId),
  ok.

get_current_time() ->
  {_,CurrentTime,_} = erlang:timestamp(),
  CurrentTime.




