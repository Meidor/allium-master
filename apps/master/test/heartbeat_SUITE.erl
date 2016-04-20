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
  meck:new(heartbeat_monitor, [non_strict, passthrough]),
  meck:new(redis, [non_strict, passthrough]),
  meck:expect(heartbeat_monitor, get_current_time, fun() -> 100000 end),
  meck:expect(node_service, verify_node, fun(NodeId, _) ->
    case NodeId of
      "10" -> ok;
      _ -> {error, "Node id and secret hash do not match"} end
                                         end),
  meck:expect(node_service, node_unregister, fun(_) -> ok end),
  meck:expect(redis, set, fun(_, _) -> ok end),
  meck:expect(redis, remove, fun(_) -> ok end),
  SecretHash = "NODE12345SECRETHASHDONTTELLANYONE",
  TimeBetweenHeartbeats = 5000,
  CurrentTime = 100000,
  [{secrethash, SecretHash}, {time, CurrentTime}, {timebetweenheartbeats, TimeBetweenHeartbeats}] ++ Config.

end_per_testcase(_, Config) ->
  Config.

receive_heartbeat_valid_node_test(Config) ->
  SecretHash = ?config(secrethash, Config),

  ok = heartbeat_monitor:receive_heartbeat("10", SecretHash),
  true = test_helpers:check_function_called(node_service, verify_node, ["10","NODE12345SECRETHASHDONTTELLANYONE"]),
  true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
  true = test_helpers:check_function_called(redis, set, ["heartbeat_node_10",100000]).

receive_heartbeat_invalid_node_test(Config) ->
  SecretHash = ?config(secrethash, Config),

  {error, "Node id and secret hash do not match"} = heartbeat_monitor:receive_heartbeat("11", SecretHash),
  true = test_helpers:check_function_called(node_service, verify_node, ["11","NODE12345SECRETHASHDONTTELLANYONE"]).

remove_nodes_with_inactive_heartbeats_test(Config) ->
  Time = ?config(time, Config),
  TimeBetweenHeartbeats = ?config(timebetweenheartbeats, Config),
  MonitoredNodes = [<<"onion_heartbeat_node_10">>, <<"onion_heartbeat_node_11">>, <<"onion_heartbeat_node_12">>, <<"onion_heartbeat_node_13">>],
  MonitoredTimes = [integer_to_binary(Time-TimeBetweenHeartbeats), integer_to_binary(Time-TimeBetweenHeartbeats-1),
    integer_to_binary(Time), integer_to_binary(Time-TimeBetweenHeartbeats*2)],
  meck:expect(redis, get_matching_keys, fun("heartbeat_node_") -> MonitoredNodes end),
  meck:expect(redis, get_list, fun(_) -> MonitoredTimes end),

  ["11", "13"] = heartbeat_monitor:remove_inactive_nodes(TimeBetweenHeartbeats),
  true = test_helpers:check_function_called(redis, get_matching_keys, ["heartbeat_node_"]),
  true = test_helpers:check_function_called(redis, get_list, [["onion_heartbeat_node_10", "onion_heartbeat_node_11",
    "onion_heartbeat_node_12", "onion_heartbeat_node_13"]]),
  true = test_helpers:check_function_called(redis, remove, ["heartbeat_node_11"]),
  true = test_helpers:check_function_called(redis, remove, ["heartbeat_node_13"]),
  true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
  true = test_helpers:check_function_called(node_service, node_unregister, ["11"]),
  true = test_helpers:check_function_called(node_service, node_unregister, ["13"]).

add_node_to_receive_heartbeat_format(_) ->
  ok = heartbeat_monitor:add_node("12"),
  true = test_helpers:check_function_called(redis, set, ["heartbeat_node_12", 100000]).

remove_node_from_heartbeat_monitor(_) ->
  ok = heartbeat_monitor:remove_node("20"),
  true = test_helpers:check_function_called(redis, remove, ["heartbeat_node_20"]).
