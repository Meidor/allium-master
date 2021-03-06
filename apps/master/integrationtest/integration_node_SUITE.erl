-module(integration_node_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_suite/1
]).

-export([
    update_a_non_existing_node_return_error_test/1,
    delete_a_non_existing_node_return_error_test/1,
    register_a_node_return_success_message_test/1,
    register_already_registered_node_return_error_test/1,
    register_node_with_wrong_ip_return_error_test/1,
    get_graph_updates_return_update_with_registered_node_test/1,
    update_a_nodes_public_key_return_success_message_test/1,
    update_a_nodes_ip_port_return_success_message_test/1,
    update_a_nodes_with_wrong_ip_return_error_message_test/1,
    get_graph_updates_after_inserting_updating_and_deleting_node_return_updates_test/1,
    get_graph_updates_from_version_3_after_inserting_updating_and_deleting_node_return_updates_test/1,
    get_graph_updates_from_max_version_after_inserting_updating_and_deleting_node_return_no_updates_test/1,
    delete_node_return_succes_message_test/1,
    rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test/1,
    node_is_removed_after_not_sending_heartbeat_test/1,
    send_heartbeat_from_existing_node_return_ok_test/1,
    send_heartbeat_from_non_existing_node_return_error_test/1
]).

all() -> [
    update_a_non_existing_node_return_error_test,
    delete_a_non_existing_node_return_error_test,
    register_a_node_return_success_message_test,
    register_already_registered_node_return_error_test,
    register_node_with_wrong_ip_return_error_test,
    get_graph_updates_return_update_with_registered_node_test,
    update_a_nodes_public_key_return_success_message_test,
    update_a_nodes_ip_port_return_success_message_test,
    update_a_nodes_with_wrong_ip_return_error_message_test,
    get_graph_updates_after_inserting_updating_and_deleting_node_return_updates_test,
    get_graph_updates_from_version_3_after_inserting_updating_and_deleting_node_return_updates_test,
    get_graph_updates_from_max_version_after_inserting_updating_and_deleting_node_return_no_updates_test,
    delete_node_return_succes_message_test,
    rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test,
    node_is_removed_after_not_sending_heartbeat_test,
    send_heartbeat_from_existing_node_return_ok_test,
    send_heartbeat_from_non_existing_node_return_error_test
].

init_per_suite(Config) ->
    application:load(master),
    application:load(websocket),
    application:ensure_all_started(websocket),
    test_helpers_int:init_sharded_eredis(),
    persistence_service:init(),

    ValidNodeId = "192.122.1.1:80",
    ValidNodeSecretHash = "secrethash12345",
    ValidNodeIP = "192.122.1.1",
    ValidNodePort = 80,
    ValidNodePublicKey = <<"generatedpublickey">>,
    OtherNodeId = "255.255.0.1:50",
    OtherIP = "255.255.0.1",
    OtherPort = 50,
    OtherPublicKey = <<"otherpublickey">>,
    [
        {node, {ValidNodeId, ValidNodeSecretHash, ValidNodeIP, ValidNodePort, ValidNodePublicKey}},
        {othernode, {OtherNodeId, ValidNodeSecretHash, OtherIP, OtherPort, OtherPublicKey}}
    ] ++ Config.

init_per_testcase(_, Config) ->
    test_helpers_int:empty_database(),
    Config.

end_per_suite(Config) ->
    application:unload(master),
    application:unload(websocket),
    Config.

update_a_non_existing_node_return_error_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = ?config(node, Config),
    Request = {nodeupdaterequest, NodeId, SecretHash, IP, Port, PublicKey},

    {nodeupdateresponse, 'FAILED', undefined} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')).

delete_a_non_existing_node_return_error_test(Config) ->
    {NodeId, SecretHash, _, _, _} = ?config(node, Config),
    Request = {nodedeleterequest, NodeId, SecretHash},

    {nodedeleteresponse, 'FAILED'} = hrp_pb:decode_nodedeleteresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')).

register_a_node_return_success_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    Request = {noderegisterrequest, IP, Port, PublicKey},

    {noderegisterresponse, 'SUCCES', NodeId, SecretHash} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    true = test_helpers_int:valid_id(NodeId),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    true = is_binary(redis:get("heartbeat_node_" ++ NodeId)).

register_already_registered_node_return_error_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    Request = {noderegisterrequest, IP, Port, PublicKey},
    test_helpers_int:register_node(IP, Port, PublicKey),

    {noderegisterresponse, 'ALREADY_EXISTS', undefined, undefined} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')).

register_node_with_wrong_ip_return_error_test(Config) ->
    {_, _, _, Port, PublicKey} = ?config(node, Config),
    Request = {noderegisterrequest, "256.256.256.256", Port, PublicKey},

    {noderegisterresponse, 'FAILED', undefined, undefined} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')).

get_graph_updates_return_update_with_registered_node_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NodeId, _SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {graphupdaterequest, 0},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphOneUpdates = [hrp_pb:decode_graphupdate(GraphOneUpdate) || GraphOneUpdate <- GraphUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []}
    ] = DecodedGraphOneUpdates.

update_a_nodes_public_key_return_success_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {_, _, _, _, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {nodeupdaterequest, NodeId, SecretHash, IP, Port, NewPublicKey},

    {nodeupdateresponse, 'SUCCES', NodeId} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')).

update_a_nodes_ip_port_return_success_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NewNodeId, _, NewIP, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {nodeupdaterequest, NodeId, SecretHash, NewIP, NewPort, NewPublicKey},

    {nodeupdateresponse, 'SUCCES', NewNodeId} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')).

update_a_nodes_with_wrong_ip_return_error_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {_, _, _, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {nodeupdaterequest, NodeId, SecretHash, "256.256.256.256", NewPort, NewPublicKey},

    {nodeupdateresponse, 'FAILED', undefined} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')).

get_graph_updates_after_inserting_updating_and_deleting_node_return_updates_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NewNodeId, _, NewIP, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    {NewNodeId, SecretHash, NewIP, NewPort, NewPublicKey} =
        test_helpers_int:update_node(NodeId, SecretHash, NewIP, NewPort, NewPublicKey),
    test_helpers_int:delete_node(NewNodeId, SecretHash),
    Request = {graphupdaterequest, 0},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []},
        {graphupdate, 3, false, [], [{node, NodeId, [] , 0, <<>>, []}]},
        {graphupdate, 4, false, [{node, NewNodeId, NewIP, NewPort, NewPublicKey, []}], []},
        {graphupdate, 5, false, [], [{node, NewNodeId, [] , 0, <<>>, []}]}
    ] = DecodedGraphUpdates.

get_graph_updates_from_version_3_after_inserting_updating_and_deleting_node_return_updates_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NewNodeId, _, NewIP, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    {NewNodeId, SecretHash, NewIP, NewPort, NewPublicKey} =
        test_helpers_int:update_node(NodeId, SecretHash, NewIP, NewPort, NewPublicKey),
    test_helpers_int:delete_node(NewNodeId, SecretHash),
    Request = {graphupdaterequest, 3},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 4, false, [{node, NewNodeId, NewIP, NewPort, NewPublicKey, []}], []},
        {graphupdate, 5, false, [], [{node, NewNodeId, [] , 0, <<>>, []}]}
    ] = DecodedGraphUpdates.

get_graph_updates_from_max_version_after_inserting_updating_and_deleting_node_return_no_updates_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NewNodeId, _, NewIP, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    {NewNodeId, SecretHash, NewIP, NewPort, NewPublicKey} =
        test_helpers_int:update_node(NodeId, SecretHash, NewIP, NewPort, NewPublicKey),
    test_helpers_int:delete_node(NewNodeId, SecretHash),
    Request = {graphupdaterequest, 5},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [] = DecodedGraphUpdates.

delete_node_return_succes_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {nodedeleterequest, NodeId, SecretHash},

    {nodedeleteresponse, 'SUCCES'} = hrp_pb:decode_nodedeleteresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')).

rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NewNodeId, _, NewIP, NewPort, NewPublicKey} = ?config(othernode, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:register_node(IP, Port, PublicKey),
    {NewNodeId, SecretHash, NewIP, NewPort, NewPublicKey} =
        test_helpers_int:update_node(NodeId, SecretHash, NewIP, NewPort, NewPublicKey),
    test_helpers_int:delete_node(NewNodeId, SecretHash),

    graph_monitor_sup:start_link(),
    timer:sleep(31000),

    Request = {graphupdaterequest, 0},
    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 3, true, [], []},
        {graphupdate, 4, false, [{node, NewNodeId, NewIP, NewPort, NewPublicKey, []}], []},
        {graphupdate, 5, false,[], [{node, NewNodeId, [], 0, <<>>, []}]}
    ] = DecodedGraphUpdates.

node_is_removed_after_not_sending_heartbeat_test(Config) ->
    heartbeat_monitor_sup:start_link(),
    graph_monitor_sup:start_link(),

    {_, _, IPOne, PortOne, PublicKeyOne} = ?config(node, Config),
    {_, _, IPTwo, PortTwo, PublicKeyTwo} = ?config(othernode, Config),

    {NodeIdOne, SecretHashOne, IPOne, PortOne, PublicKeyOne} =
        test_helpers_int:register_node(IPOne, PortOne, PublicKeyOne),
    {NodeIdTwo, _SecretHashTwo, IPTwo, PortTwo, PublicKeyTwo} =
        test_helpers_int:register_node(IPTwo, PortTwo, PublicKeyTwo),

    HeartbeatRequest = test_helpers_int:encode_message_to_binary({nodeheartbeat, NodeIdOne, SecretHashOne}),

    timer:sleep(7000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'NODEHEARTBEAT'),
    timer:sleep(7000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'NODEHEARTBEAT'),
    timer:sleep(7000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'NODEHEARTBEAT'),

    true = is_binary(redis:get("heartbeat_node_" ++ NodeIdOne)),
    undefined = redis:get("heartbeat_node_" ++ NodeIdTwo),

    timer:sleep(10000),

    Request = {graphupdaterequest, 0},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 3, true, [
            {node, NodeIdOne, IPOne, PortOne, PublicKeyOne, []},
            {node, NodeIdTwo, IPTwo, PortTwo, PublicKeyTwo, []}
        ], []},
        {graphupdate, 4, false, [], [{node, NodeIdTwo, [], 0, <<>>, []}]}
    ] = DecodedGraphUpdates.

send_heartbeat_from_existing_node_return_ok_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    {NodeId, SecretHash, IP, Port, PublicKey} =
        test_helpers_int:register_node(IP, Port, PublicKey),
    Request = {nodeheartbeat, NodeId, SecretHash},

    Message = hrp_pb:encode(
        [{wrapper, 'NODEHEARTBEAT', test_helpers_int:encode_message_to_binary(Request)}]
    ),

    {ok,<<"OK">>} = master_app:handle_message(Message).

send_heartbeat_from_non_existing_node_return_error_test(_Config) ->
    Request = {nodeheartbeat, "InvalidNodeId", "SecretHash"},

    Message = hrp_pb:encode(
        [{wrapper, 'NODEHEARTBEAT', test_helpers_int:encode_message_to_binary(Request)}]
    ),

    test_helpers:assert_fail(fun master_app:handle_message/1, [Message], error, nodenotverified, failed_to_catch_invalid_node).