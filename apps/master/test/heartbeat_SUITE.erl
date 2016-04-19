-module(heartbeat_SUITE).
-author("Koen & Niels").

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([receive_heartbeat_valid_node_test/1, receive_heartbeat_invalid_node_test/1, remove_nodes_with_inactive_heartbeats_test/1, add_node_to_receive_heartbeat_format/1, remove_node_from_heartbeat_monitor/1]).

all() -> [receive_heartbeat_valid_node_test, receive_heartbeat_invalid_node_test, remove_nodes_with_inactive_heartbeats_test, add_node_to_receive_heartbeat_format, remove_node_from_heartbeat_monitor].

init_per_testcase(_, Config) ->
  ExistingId = "10",
  NonExistingId = "11",
  NewNodeId = "12",
  SecretHash = "NODE12345SECRETHASHDONTTELLANYONE",
  ValidNode = {ExistingId, SecretHash},
  InvalidNode = {NonExistingId, SecretHash},
  RemovedNode = "heartbeat_node_20",
  TimeBetweenHeartbeats = 5000,
  CurrentTime = 100000,
  MonitoredNodes = [<<"onion_heartbeat_node_10">>, <<"onion_heartbeat_node_11">>, <<"onion_heartbeat_node_12">>, <<"onion_heartbeat_node_13">>],
  MonitoredTimes = [integer_to_binary(CurrentTime-TimeBetweenHeartbeats), integer_to_binary(CurrentTime-TimeBetweenHeartbeats-1), integer_to_binary(CurrentTime), integer_to_binary(CurrentTime-TimeBetweenHeartbeats*2)],
  meck:new(node_service, [non_strict, passthrough]),
  meck:new(heartbeat_monitor, [non_strict, passthrough]),
  meck:new(redis, [non_strict, passthrough]),
  meck:expect(heartbeat_monitor, get_current_time, fun() -> 100000 end),
  meck:expect(node_service, verify_node, fun(NodeId, _) ->
                                         case NodeId of
                                           "10" -> ok;
                                           _ -> {error, "Node id and secret hash do not match"} end
                                         end),
  meck:expect(node_service, node_unregister, fun(_) -> ok end),
  meck:expect(redis, get_matching_keys, fun("heartbeat_node_") -> MonitoredNodes end),
  meck:expect(redis, get_list, fun(MonitoredNodes) -> MonitoredTimes end),
  meck:expect(redis, set, fun(_, _) -> ok end),
  meck:expect(redis, remove, fun(_) -> ok end),
  [{validnode, ValidNode},  {invalidnode, InvalidNode}, {time, TimeBetweenHeartbeats}, {newnode, NewNodeId}, {removednode, RemovedNode}] ++ Config.

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
  ["11", "13"] = heartbeat_monitor:remove_inactive_nodes(Time).

add_node_to_receive_heartbeat_format(Config) ->
  NodeId = ?config(newnode, Config),
  ok = heartbeat_monitor:add_node(NodeId),
  true = test_helpers:check_function_called(redis, set, ["heartbeat_node_12", 100000]).

remove_node_from_heartbeat_monitor(Config) ->
  NodeKey = ?config(removednode, Config),
  ok = heartbeat_monitor:remove_node(NodeKey).
