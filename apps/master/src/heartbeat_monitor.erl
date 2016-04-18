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
-export([receive_heartbeat/2, remove_inactive_nodes/1, add_node/1]).

receive_heartbeat(NodeId, SecretHash) ->
  Node_verified = node_service:verify_node(NodeId, SecretHash),
  case Node_verified of
    ok ->
      {_,CurrentTime,_} = erlang:timestamp(),
      redis:set("heartbeat_node_" ++ NodeId, CurrentTime),
      ok;
    _ ->
      {error, "Node id and secret hash do not match"}
  end.

remove_inactive_nodes(TimeBetweenHeartbeats) ->
  AllKeys = [binary_to_list(Key) || Key <- redis:get_matching_keys("heartbeat_node_")],
  AllValues = [binary_to_integer(Value) || Value <- redis:get_list(AllKeys)],
  {_,CurrentTime,_} = erlang:timestamp(),
  AllNodes = [Key || {Key, Value} <- lists:zip(AllKeys, AllValues), Value < (CurrentTime - TimeBetweenHeartbeats)],
  RemovedNodes = [string:substr(Key, 22)  || Key <- AllNodes],
  lists:foreach(fun(Node) -> node_service:node_unregister(Node) end, RemovedNodes),
  RemovedNodes.

add_node(NodeId) ->
  {_,CurrentTime,_} = erlang:timestamp(),
  redis:set("heartbeat_node_" ++ NodeId, CurrentTime),
  ok.





