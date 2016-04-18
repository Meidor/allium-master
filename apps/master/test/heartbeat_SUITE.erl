-module(heartbeat_SUITE).
-author("Koen & Niels").

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([receive_heartbeat_valid_node_test/1, receive_heartbeat_invalid_node_test/1, remove_nodes_with_inactive_heartbeats_test/1, add_node_to_receive_heartbeat_format/1]).

all() -> [receive_heartbeat_valid_node_test, receive_heartbeat_invalid_node_test, remove_nodes_with_inactive_heartbeats_test, add_node_to_receive_heartbeat_format].

init_per_testcase(_, Config) ->
  ExistingId = "10",
  NonExistingId = "11",
  NewNodeId = "12",
  SecretHash = "NODE12345SECRETHASHDONTTELLANYONE",
  ValidNode = {ExistingId, SecretHash},
  InvalidNode = {NonExistingId, SecretHash},
  TimeBetweenHeartbeats = 5000,
  meck:new(node_service, [non_strict, passthrough]),
  meck:expect(node_service, verify_node, fun(NodeId, _) ->
                                         case NodeId of "10" -> ok;
                                                        _ -> {error, "Node id and secret hash do not match"} end
                                         end),
  meck:expect(node_service, node_unregister, fun(_) -> ok end),
  [{validnode, ValidNode},  {invalidnode, InvalidNode}, {time, TimeBetweenHeartbeats}, {newnode, NewNodeId}] ++ Config.

end_per_testcase(receive_heartbeat_test, Config) ->
  Config.

receive_heartbeat_valid_node_test(Config) ->
  {NodeId, SecretHash} = ?config(validnode, Config),
  ok = heartbeat_monitor:receive_heartbeat(NodeId, SecretHash).

receive_heartbeat_invalid_node_test(Config) ->
  {NodeId, SecretHash} = ?config(invalidnode, Config),
  {error, "Node id and secret hash do not match"} = heartbeat_monitor:receive_heartbeat(NodeId, SecretHash).

remove_nodes_with_inactive_heartbeats_test(Config) ->
  Time = ?config(time, Config),
  {_,CurrentTime,_} = erlang:timestamp(),
  redis:set("heartbeat_node_10", integer_to_list(CurrentTime-Time)),
  redis:set("heartbeat_node_11", integer_to_list(CurrentTime-Time-1)),
  redis:set("heartbeat_node_12", integer_to_list(CurrentTime)),
  redis:set("heartbeat_node_13", integer_to_list(CurrentTime-Time*2)),
  ["11", "13"] = heartbeat_monitor:remove_inactive_nodes(Time).

add_node_to_receive_heartbeat_format(Config) ->
  NodeId = ?config(newnode, Config),
  ok = heartbeat_monitor:add_node(NodeId).