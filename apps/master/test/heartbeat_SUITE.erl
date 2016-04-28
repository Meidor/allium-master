-module(heartbeat_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([receive_heartbeat_node_valid_node_test/1, 
    receive_heartbeat_client_valid_client_test/1,
    receive_heartbeat_node_invalid_node_test/1,
    receive_heartbeat_client_invalid_client_test/1, 
    remove_nodes_with_inactive_heartbeats_test/1,
    remove_clients_with_inactive_heartbeats_test/1,
    add_node_to_receive_heartbeat_node_format/1, 
    add_client_to_receive_heartbeat_client_format/1, 
    remove_node_from_heartbeat_monitor/1,
    remove_client_from_heartbeat_monitor/1]).

all() ->
    [receive_heartbeat_node_valid_node_test, 
    receive_heartbeat_client_valid_client_test, 
    receive_heartbeat_node_invalid_node_test, 
    receive_heartbeat_client_invalid_client_test, 
    remove_nodes_with_inactive_heartbeats_test,
    remove_clients_with_inactive_heartbeats_test,
    add_node_to_receive_heartbeat_node_format, 
    add_client_to_receive_heartbeat_client_format, 
    remove_node_from_heartbeat_monitor,
    remove_client_from_heartbeat_monitor].

init_per_testcase(_, Config) ->
    meck:new(node_service, [non_strict, passthrough]),
    meck:new(client_service, [non_strict, passthrough]),
    meck:new(heartbeat_monitor, [passthrough]),
    meck:new(redis, [non_strict, passthrough]),
    meck:expect(heartbeat_monitor, get_current_time, fun() -> 100000 end),
    meck:expect(node_service, node_verify,
        fun(NodeId, _) ->
            case NodeId of
                "10" -> ok;
                _ -> error(nodenotfound) end
        end),
    meck:expect(client_service, client_verify,
        fun(Username, _) ->
            case Username of
                "ValidUser" -> ok;
                _ -> error(clientnotfound) end
        end),
    meck:expect(node_service, node_unregister, fun(_) -> ok end),
    meck:expect(redis, set, fun(_, _) -> ok end),
    meck:expect(redis, remove, fun(_) -> ok end),
    SecretHash = "NODE12345SECRETHASHDONTTELLANYONE",
    ValidPass = "ValidPass",
    TimeBetweenHeartbeats = 5000,
    CurrentTime = 100000,
    HeartbeatNodeLabel = "heartbeat_node_",
    HeartbeatClientLabel = "heartbeat_client_",
    NodeLabel = "onion_" ++ HeartbeatNodeLabel,
    NodeLabels = {HeartbeatNodeLabel, NodeLabel},
    ClientLabel = "onion_" ++ HeartbeatClientLabel,
    ClientLabels = {HeartbeatClientLabel, ClientLabel},

    [{secrethash, SecretHash}, {validpass, ValidPass}, {time, CurrentTime}, {timebetweenheartbeats, TimeBetweenHeartbeats}, {nodelabels, NodeLabels}, {clientlabels, ClientLabels}] ++ Config.

end_per_testcase(_, Config) ->
    meck:unload(redis),
    meck:unload(heartbeat_monitor),
    meck:unload(node_service),
    meck:unload(client_service),
    Config.

receive_heartbeat_node_valid_node_test(Config) ->
    SecretHash = ?config(secrethash, Config),
    {HeartbeatNodeLabel, _} = ?config(nodelabels, Config),
    NodeId = "10",

    ok = heartbeat_monitor:receive_heartbeat_node(NodeId, SecretHash),
    true = test_helpers:check_function_called(node_service, node_verify, [NodeId, SecretHash]),
    true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
    true = test_helpers:check_function_called(redis, set, [HeartbeatNodeLabel ++ NodeId, 100000]).

receive_heartbeat_client_valid_client_test(Config) ->
    Validpass = ?config(validpass, Config),
    {HeartbeatClientLabel, _} = ?config(clientlabels, Config),
    Username = "ValidUser",

    ok = heartbeat_monitor:receive_heartbeat_client(Username, Validpass),
    true = test_helpers:check_function_called(client_service, client_verify, [Username, Validpass]),
    true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
    true = test_helpers:check_function_called(redis, set, [HeartbeatClientLabel ++ Username, 100000]).

receive_heartbeat_node_invalid_node_test(Config) ->
    SecretHash = ?config(secrethash, Config),
    NodeId = "11",

    {error, "Node id and secret hash do not match"} = heartbeat_monitor:receive_heartbeat_node(NodeId, SecretHash),
    true = test_helpers:check_function_called(node_service, node_verify, [NodeId, SecretHash]).

receive_heartbeat_client_invalid_client_test(Config) ->
    Validpass = ?config(validpass, Config),
    Username = "InvalidUsername",

    {error, "username and Secret hash do not match"} = heartbeat_monitor:receive_heartbeat_client(Username, Validpass),
    true = test_helpers:check_function_called(client_service, client_verify, [Username, Validpass]).

remove_nodes_with_inactive_heartbeats_test(Config) ->
    Time = ?config(time, Config),
    TimeBetweenHeartbeats = ?config(timebetweenheartbeats, Config),
    {HeartbeatNodeLabel, NodeLabel} = ?config(nodelabels, Config),
    InactiveNodeIdOne = "11",
    InactiveNodeIdTwo = "13",
    ActiveNodeIdOne = "10",
    ActiveNodeIdTwo = "12",

    MonitoredNodes = [list_to_binary(NodeLabel ++ ActiveNodeIdOne), list_to_binary(NodeLabel ++ InactiveNodeIdOne),
        list_to_binary(NodeLabel ++ ActiveNodeIdTwo), list_to_binary(NodeLabel ++ InactiveNodeIdTwo)],
    MonitoredTimes = [integer_to_binary(Time - TimeBetweenHeartbeats), integer_to_binary(Time - TimeBetweenHeartbeats - 1),
        integer_to_binary(Time), integer_to_binary(Time - TimeBetweenHeartbeats * 2)],
    meck:expect(redis, get_matching_keys, fun(_HeartbeatNodeLabel) -> MonitoredNodes end),
    meck:expect(redis, get_list, fun(_) -> MonitoredTimes end),

    [InactiveNodeIdOne, InactiveNodeIdTwo] = heartbeat_monitor:remove_inactive_nodes(TimeBetweenHeartbeats),
    true = test_helpers:check_function_called(redis, get_matching_keys, [HeartbeatNodeLabel]),
    true = test_helpers:check_function_called(redis, get_list, [[NodeLabel ++ ActiveNodeIdOne, NodeLabel ++ InactiveNodeIdOne,
            NodeLabel ++ ActiveNodeIdTwo, NodeLabel ++ InactiveNodeIdTwo]]),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ InactiveNodeIdOne]),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ InactiveNodeIdTwo]),
    true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
    true = test_helpers:check_function_called(node_service, node_unregister, [InactiveNodeIdOne]),
    true = test_helpers:check_function_called(node_service, node_unregister, [InactiveNodeIdTwo]).

remove_clients_with_inactive_heartbeats_test(Config) ->
    Time = ?config(time, Config),
    TimeBetweenHeartbeats = ?config(timebetweenheartbeats, Config),
    {HeartbeatClientLabel, ClientLabel} = ?config(clientlabels, Config),
    InactiveClientIdOne = "client11",
    InactiveClientIdTwo = "client12",
    ActiveClientIdOne = "client13",
    ActiveClientIdTwo = "cleint14",

    MonitoredClients = [list_to_binary(ClientLabel ++ ActiveClientIdOne), list_to_binary(ClientLabel ++ InactiveClientIdOne),
        list_to_binary(ClientLabel ++ ActiveClientIdTwo), list_to_binary(ClientLabel ++ InactiveClientIdTwo)],
    MonitoredTimes = [integer_to_binary(Time - TimeBetweenHeartbeats), integer_to_binary(Time - TimeBetweenHeartbeats - 1),
        integer_to_binary(Time), integer_to_binary(Time - TimeBetweenHeartbeats * 2)],
    meck:expect(redis, get_matching_keys, fun(_HeartbeatClientLabel) -> MonitoredClients end),
    meck:expect(redis, get_list, fun(_) -> MonitoredTimes end),
    meck:expect(client_service, client_logout, fun(_) -> ok end),

    [InactiveClientIdOne, InactiveClientIdTwo] = heartbeat_monitor:remove_inactive_clients(TimeBetweenHeartbeats),
    true = test_helpers:check_function_called(redis, get_matching_keys, [HeartbeatClientLabel]),
    true = test_helpers:check_function_called(redis, get_list, [[ClientLabel ++ ActiveClientIdOne, ClientLabel ++ InactiveClientIdOne,
            ClientLabel ++ ActiveClientIdTwo, ClientLabel ++ InactiveClientIdTwo]]),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatClientLabel ++ InactiveClientIdOne]),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatClientLabel ++ InactiveClientIdTwo]),
    true = test_helpers:check_function_called(heartbeat_monitor, get_current_time, []),
    true = test_helpers:check_function_called(client_service, client_logout, [InactiveClientIdOne]),
    true = test_helpers:check_function_called(client_service, client_logout, [InactiveClientIdTwo]).

add_node_to_receive_heartbeat_node_format(Config) ->
    {HeartbeatNodeLabel, _} = ?config(nodelabels, Config),
    NodeId = "12",

    ok = heartbeat_monitor:add_node(NodeId),
    true = test_helpers:check_function_called(redis, set, [HeartbeatNodeLabel ++ NodeId, 100000]).

add_client_to_receive_heartbeat_client_format(Config) ->
    {HeartbeatClientLabel, _} = ?config(clientlabels, Config),
    Username = "validUser",

    ok = heartbeat_monitor:add_client(Username),
    true = test_helpers:check_function_called(redis, set, [HeartbeatClientLabel ++ Username, 100000]).

remove_node_from_heartbeat_monitor(Config) ->
    {HeartbeatNodeLabel, _} = ?config(nodelabels, Config),
    NodeId = "20",

    ok = heartbeat_monitor:remove_node(NodeId),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatNodeLabel ++ NodeId]).

remove_client_from_heartbeat_monitor(Config) ->
    {HeartbeatClientLabel, _} = ?config(clientlabels, Config),
    Username = "validUser",

    ok = heartbeat_monitor:remove_client(Username),
    true = test_helpers:check_function_called(redis, remove, [HeartbeatClientLabel ++ Username]).


