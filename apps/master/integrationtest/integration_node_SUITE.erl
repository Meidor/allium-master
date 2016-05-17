-module(integration_node_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    update_a_non_existing_node_return_error_test/1,
    delete_a_non_existing_node_return_error_test/1,
    send_heartbeat_from_non_existing_node_do_nothing_test/1,
    register_a_node_return_success_message_test/1,
    register_already_registered_node_return_error_test/1,
    get_graph_updates_return_update_with_registered_node_test/1,
    update_a_node_return_success_message_test/1,
    get_graph_updates_return_updates_deleting_and_adding_updated_node_test/1,
    delete_node_return_succes_message_test/1,
    get_graph_updates_from_version_three_on_return_adding_deleting_updated_node_test/1,
    rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test/1,
    node_is_removed_after_not_sending_heartbeat_test/1,
    rebuild_graph_return_graph_with_graphupdate_six_as_full_graph_and_node_without_heartbeat_deleted_test/1
]).

all() -> [
    update_a_non_existing_node_return_error_test,
    delete_a_non_existing_node_return_error_test,
    send_heartbeat_from_non_existing_node_do_nothing_test,
    register_a_node_return_success_message_test,
    register_already_registered_node_return_error_test,
    get_graph_updates_return_update_with_registered_node_test,
    update_a_node_return_success_message_test,
    get_graph_updates_return_updates_deleting_and_adding_updated_node_test,
    delete_node_return_succes_message_test,
    get_graph_updates_from_version_three_on_return_adding_deleting_updated_node_test,
    rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test,
    node_is_removed_after_not_sending_heartbeat_test,
    rebuild_graph_return_graph_with_graphupdate_six_as_full_graph_and_node_without_heartbeat_deleted_test
].

init_per_suite(Config) ->
    test_helpers_int:empty_database(),
    master_sup:start_link(),

    InvalidNodeId = "12345",
    InvalidNodeSecretHash = "secrethash12345",
    ValidNodeIP = "255.255.255.255",
    ValidNodePort = 80,
    ValidNodePublicKey = <<"generatedpublickey">>,
    [{node, {InvalidNodeId, InvalidNodeSecretHash, ValidNodeIP, ValidNodePort, ValidNodePublicKey}}] ++ Config.

end_per_suite(Config) ->
    Config.

update_a_non_existing_node_return_error_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = ?config(node, Config),
    Request = {nodeupdaterequest, NodeId, SecretHash, IP, Port, PublicKey},

    {nodeupdateresponse, 'FAILED'} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')).

delete_a_non_existing_node_return_error_test(Config) ->
    {NodeId, SecretHash, _, _, _} = ?config(node, Config),
    Request = {nodedeleterequest, NodeId, SecretHash},

    {nodedeleteresponse, 'FAILED'} = hrp_pb:decode_nodedeleteresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')).

send_heartbeat_from_non_existing_node_do_nothing_test(Config) ->
    {NodeId, SecretHash, _, _, _} = ?config(node, Config),
    Request = test_helpers_int:encode_message_to_binary({nodeheartbeat, NodeId, SecretHash}),

    try
        test_helpers_int:send_heartbeat(Request, 'NODEHEARTBEAT'),
        error(shouldnotbereached)
    catch
        error:nodenotverified ->
            undefined = redis:get("heartbeat_node_" ++ NodeId);
        _:_ ->
            error(testdidnotpass)
    end.

register_a_node_return_success_message_test(Config) ->
    {_, _, IP, Port, PublicKey} = ?config(node, Config),
    Request = {noderegisterrequest, IP, Port, PublicKey},

    {noderegisterresponse, 'SUCCES', NodeId, SecretHash} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    true = test_helpers_int:valid_id(NodeId),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    true = is_binary(redis:get("heartbeat_node_" ++ NodeId)),

    test_helpers_int:pass_to_next_test([{registerednode, {NodeId, SecretHash, IP, Port, PublicKey}}]).

register_already_registered_node_return_error_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(registerednode, Config),
%%    Request = {noderegisterrequest, IP, Port, PublicKey},

%%    {noderegisterresponse, 'ALREADY_EXISTS', "", ""} = hrp_pb:decode_noderegisterresponse(
%%        test_helpers_int:get_data_encrypted_response(Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),

    {skip_and_save, "An already registered node can still register at this time",
        [{registerednode, {NodeId, SecretHash, IP, Port, PublicKey}}]}.

get_graph_updates_return_update_with_registered_node_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(registerednode, Config),
    OwnVersionGraph = 0,
    Request = {graphupdaterequest, OwnVersionGraph},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphOneUpdates = [hrp_pb:decode_graphupdate(GraphOneUpdate) || GraphOneUpdate <- GraphUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []}
    ] = DecodedGraphOneUpdates,
    test_helpers_int:pass_to_next_test([{registerednode, {NodeId, SecretHash, IP, Port, PublicKey}}]).

update_a_node_return_success_message_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(registerednode, Config),
    NewIP = "255.255.255.100",
    NewPort = 50,
    NewPublicKey = <<"newlygeneratedpublickey">>,
    Request = {nodeupdaterequest, NodeId, SecretHash, NewIP, NewPort, NewPublicKey},

    {nodeupdateresponse, 'SUCCES'} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')),

    test_helpers_int:pass_to_next_test([
        {registerednode, {NodeId, SecretHash, IP, Port, PublicKey}},
        {updatednode, {NodeId, SecretHash, NewIP, NewPort, NewPublicKey}}]).

get_graph_updates_return_updates_deleting_and_adding_updated_node_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(registerednode, Config),
    {NodeId, SecretHash, NewIP, NewPort, NewPublicKey} = test_helpers_int:retrieve_from_last_test(updatednode, Config),
    OwnVersionGraph = 0,
    Request = {graphupdaterequest, OwnVersionGraph},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []},
        {graphupdate, 3, false, [], [{node, NodeId, [] , 0, <<>>, []}]},
        {graphupdate, 4, false, [{node, NodeId, NewIP, NewPort, NewPublicKey, []}], []}
    ] = DecodedGraphUpdates,

    test_helpers_int:pass_to_next_test([{updatednode, {NodeId, SecretHash, NewIP, NewPort, NewPublicKey}}]).

delete_node_return_succes_message_test(Config) ->
    {NodeId, SecretHash, NewIP, NewPort, NewPublicKey} = test_helpers_int:retrieve_from_last_test(updatednode, Config),

    Request = {nodedeleterequest, NodeId, SecretHash},
    {nodedeleteresponse, 'SUCCES'} = hrp_pb:decode_nodedeleteresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')),

    test_helpers_int:pass_to_next_test([{deletednode, {NodeId, SecretHash, NewIP, NewPort, NewPublicKey}}]).

get_graph_updates_from_version_three_on_return_adding_deleting_updated_node_test(Config) ->
    {NodeId, SecretHash, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(deletednode, Config),
    OwnVersionGraph = 3,
    Request = {graphupdaterequest, OwnVersionGraph},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 4, false, [{node, NodeId, IP, Port, PublicKey, []}], []},
        {graphupdate, 5, false, [], [{node, NodeId, [] , 0, <<>>, []}]}
    ] = DecodedGraphUpdates,

    test_helpers_int:pass_to_next_test([{deletednode, {NodeId, SecretHash, IP, Port, PublicKey}}]).

rebuild_graph_return_graph_with_graphupdate_three_as_full_graph_test(Config) ->
    graph_monitor_sup:start_link(),
    timer:sleep(11000),

    {NodeId, _, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(deletednode, Config),
    Request = {graphupdaterequest, 0},
    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphFourUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 3, true, [], []},
        {graphupdate, 4, false, [{node, NodeId, IP, Port, PublicKey, []}], []},
        {graphupdate, 5, false,[], [{node, NodeId, [], 0, <<>>, []}]}
    ] = DecodedGraphFourUpdates.

node_is_removed_after_not_sending_heartbeat_test(Config) ->
    heartbeat_monitor_sup:start_link(),

    {_, _, IPOne, PortOne, PublicKeyOne} = ?config(node, Config),
    IPTwo = "255.255.0.1",
    PortTwo = 50,
    PublicKeyTwo = <<"otherpublickey">>,
    RequestOne = {noderegisterrequest, IPOne, PortOne, PublicKeyOne},
    RequestTwo = {noderegisterrequest, IPTwo, PortTwo, PublicKeyTwo},

    {noderegisterresponse, 'SUCCES', NodeIdOne, SecretHashOne} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(RequestOne, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    {noderegisterresponse, 'SUCCES', NodeIdTwo, SecretHashTwo} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(RequestTwo, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),

    HeartbeatRequest = test_helpers_int:encode_message_to_binary({nodeheartbeat, NodeIdOne, SecretHashOne}),

    timer:sleep(7000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'NODEHEARTBEAT'),
    timer:sleep(7000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'NODEHEARTBEAT'),
    timer:sleep(7000),

    true = is_binary(redis:get("heartbeat_node_" ++ NodeIdOne)),
    undefined = redis:get("heartbeat_node_" ++ NodeIdTwo),

    test_helpers_int:pass_to_next_test([
        {activenode, {NodeIdOne, SecretHashOne, IPOne, PortOne, PublicKeyOne}},
        {deletednode, {NodeIdTwo, SecretHashTwo, IPTwo, PortTwo, PublicKeyTwo}}]).

rebuild_graph_return_graph_with_graphupdate_six_as_full_graph_and_node_without_heartbeat_deleted_test(Config) ->
    graph_monitor_sup:start_link(),
    timer:sleep(11000),

    {NodeId, _, IP, Port, PublicKey} = test_helpers_int:retrieve_from_last_test(activenode, Config),
    {NodeIdDeleted, _, IPDeleted, PortDeleted, PublicKeyDeleted} =
        test_helpers_int:retrieve_from_last_test(deletednode, Config),
    Request = {graphupdaterequest, 0},

    {graphupdateresponse, GraphUpdates} = hrp_pb:decode_graphupdateresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphUpdates = [hrp_pb:decode_graphupdate(GraphUpdate) || GraphUpdate <- GraphUpdates],
    [
        {graphupdate, 6, true, [{node, NodeId, IP, Port, PublicKey, []}], []},
        {graphupdate, 7, false, [{node, NodeIdDeleted, IPDeleted, PortDeleted, PublicKeyDeleted, []}], []},
        {graphupdate, 8, false, [], [{node, NodeIdDeleted, [], 0, <<>>, []}]}
    ] = DecodedGraphUpdates.

