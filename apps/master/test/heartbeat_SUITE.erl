-module(heartbeat_SUITE).
-author("Koen & Niels").

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([receive_heartbeat_valid_node_test/1, receive_heartbeat_invalid_node_test/1, remove_nodes_with_inactive_heartbeats_test/1,
  add_node_to_receive_heartbeat_format/1, remove_node_from_heartbeat_monitor/1]).

all() -> [receive_heartbeat_valid_node_test, receive_heartbeat_invalid_node_test, remove_nodes_with_inactive_heartbeats_test,
  add_node_to_receive_heartbeat_format, remove_node_from_heartbeat_monitor].

init_per_testcase(_, Config) ->
  meck:new(node_service, [non_strict, passthrough]),
  meck:new(heartbeat_monitor, [passthrough]),
  meck:new(redis, [non_strict, passthrough]),
  meck:expect(heartbeat_monitor, get_current_time, fun() -> 100000 end),
  meck:expect(node_service, verify_node,
    fun(NodeId, _) ->
      case NodeId of
        "10" -> ok;
        _ -> error(nodenotfound) end
    end),
  meck:expect(node_service, node_unregister, fun(_) -> ok end),
  meck:expect(redis, set, fun(_, _) -> ok end),
  meck:expect(redis, remove, fun(_) -> ok end),
  SecretHash = "NODE12345SECRETHASHDONTTELLANYONE",
  TimeBetweenHeartbeats = 5000,
  CurrentTime = 100000,
  HeartbeatNodeLabel = "heartbeat_node_",
  Label = "onion_" ++ HeartbeatNodeLabel,
  Labels = {HeartbeatNodeLabel, Label},

  [{secrethash, SecretHash}, {time, CurrentTime}, {timebetweenheartbeats, TimeBetweenHeartbeats}, {labels, Labels}] ++ Config.

end_per_testcase(_, Config) ->
  Config.

receive_heartbeat_valid_node_test(Config) ->
  SecretHash = ?config(secrethash, Config),
  {HeartbeatNodeLabel, _} = ?config(labels, Config),
  NodeId = "10",

  ok = heartbeat_monitor:receive_heartbeat(NodeId, SecretHash),
  true = test_helpers:check_function_called(node_service, verify_node, [NodeId, SecretHash]),
  true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
  true = test_helpers:check_function_called(redis, set, [HeartbeatNodeLabel ++ NodeId, 100000]).

receive_heartbeat_invalid_node_test(Config) ->
  SecretHash = ?config(secrethash, Config),
  NodeId = "11",

  {error, "Node id and secret hash do not match"} = heartbeat_monitor:receive_heartbeat(NodeId, SecretHash),
  true = test_helpers:check_function_called(node_service, verify_node, [NodeId, SecretHash]).

remove_nodes_with_inactive_heartbeats_test(Config) ->
  Time = ?config(time, Config),
  TimeBetweenHeartbeats = ?config(timebetweenheartbeats, Config),
  {HeartbeatNodeLabel, Label} = ?config(labels, Config),
  InactiveNodeIdOne = "11",
  InactiveNodeIdTwo = "13",
  ActiveNodeIdOne = "10",
  ActiveNodeIdTwo = "12",

  MonitoredNodes = [list_to_binary(Label ++ ActiveNodeIdOne), list_to_binary(Label ++ InactiveNodeIdOne),
    list_to_binary(Label ++ ActiveNodeIdTwo), list_to_binary(Label ++ InactiveNodeIdTwo)],
  MonitoredTimes = [integer_to_binary(Time-TimeBetweenHeartbeats), integer_to_binary(Time-TimeBetweenHeartbeats-1),
    integer_to_binary(Time), integer_to_binary(Time-TimeBetweenHeartbeats*2)],
  meck:expect(redis, get_matching_keys, fun(HeartbeatNodeLabel) -> MonitoredNodes end),
  meck:expect(redis, get_list, fun(_) -> MonitoredTimes end),

  [InactiveNodeIdOne, InactiveNodeIdTwo] = heartbeat_monitor:remove_inactive_nodes(TimeBetweenHeartbeats),
  true = test_helpers:check_function_called(redis, get_matching_keys, [HeartbeatNodeLabel]),
  true = test_helpers:check_function_called(redis, get_list, [[Label ++ ActiveNodeIdOne, Label ++ InactiveNodeIdOne,
    Label ++ ActiveNodeIdTwo, Label ++ InactiveNodeIdTwo]]),
  true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ InactiveNodeIdOne]),
  true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ InactiveNodeIdTwo]),
  true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
  true = test_helpers:check_function_called(node_service, node_unregister, [InactiveNodeIdOne]),
  true = test_helpers:check_function_called(node_service, node_unregister, [InactiveNodeIdTwo]).

add_node_to_receive_heartbeat_format(Config) ->
  {HeartbeatNodeLabel, _} = ?config(labels, Config),
  NodeId = "12",

  ok = heartbeat_monitor:add_node(NodeId),
  true = test_helpers:check_function_called(redis, set, [HeartbeatNodeLabel ++ NodeId, 100000]).

remove_node_from_heartbeat_monitor(Config) ->
  {HeartbeatNodeLabel, _} = ?config(labels, Config),
  NodeId = "20",

  ok = heartbeat_monitor:remove_node(NodeId),
  true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ NodeId]).
