-module(node_service_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1]).
-export([node_register_test_valid_node/1, node_register_test_invalid_node/1, 
    node_register_test_invalid_ip/1, node_register_test_double_registration/1, 
    node_unregister_test_valid_node/1, node_unregister_test_invalid_node/1,
    node_verify_test_valid_node/1, node_verify_test_invalid_node/1,
    node_update_test_valid_node/1, node_update_test_invalid_node/1,
    node_update_test_new_ip/1,
    node_update_test_invalid_ip/1, node_update_test_undefined_values_failure_test/1]).

all() -> [node_register_test_valid_node, node_unregister_test_valid_node, 
          node_register_test_invalid_node, node_unregister_test_invalid_node, 
          node_register_test_invalid_ip, node_register_test_double_registration,
          node_verify_test_valid_node, node_verify_test_invalid_node,
          node_update_test_valid_node, node_update_test_invalid_node,
          node_update_test_new_ip,
          node_update_test_invalid_ip, node_update_test_undefined_values_failure_test].

init_per_suite(Config) ->
    IPaddress = "192.168.4.4",
    Port = 1337,
    ValidNodeId = "192.168.4.4:1337",
    PublicKey = <<"MyPublicKey">>,
    ValidNode = {IPaddress, Port, PublicKey, ValidNodeId},
    InvalidNode = {42, "Harry", 42, "InvalideId"},
    [
        {validnode,ValidNode},  
        {invalidnode, InvalidNode}, 
        {validnodeverify, {ValidNodeId, "Valide"}},
        {invalidnodeverify1, {"InvalideNode", "Invalide"}},
        {invalidnodeverify2, {42, invalide}},
        {invalidnodeverify3, {"NodeNotFound", "NotFound"}}
    ] ++ Config.
   
init_per_testcase(_, Config) ->
    {ValidNodeId, Hash} = ?config(validnodeverify, Config),
    meck:new(node_graph_manager, [non_strict]),
    %% Setup Graph Manager Mock
    meck:expect(node_graph_manager, add_node, fun(_,_,_) ->
        {ValidNodeId, Hash} end),
    meck:expect(node_graph_manager, remove_node, fun(NodeId) ->
        case NodeId of
            ValidNodeId -> ok;
            _ -> error(nodenotfound)
        end
    end),
    meck:expect(node_graph_manager, get_node_secret_hash, fun(NodeId) ->
        case NodeId of
            ValidNodeId ->  Hash;
            "InvalideNode" -> "Testing123#";
            _ -> error(nodenotfound)
        end
    end),
    meck:expect(node_graph_manager, update_node, fun(_,_,_,_,_) -> ok end),

    meck:new(heartbeat_monitor, [non_strict]),
    %% Setup Heartbeat Monitor Mock
    meck:expect(heartbeat_monitor, add_node, fun(_) -> ok end),
    meck:expect(heartbeat_monitor, remove_node, fun(NodeId) ->
        case NodeId of
            ValidNodeId -> ok;
            _ -> error(nodenotfound) 
        end 
    end),
    meck:new(redis, [non_strict]),
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "node_edges_" ++ ValidNodeId ->
                <<>>
        end
    end),
    meck:expect(redis, set, fun(_, _) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(node_graph_manager),
    meck:unload(heartbeat_monitor),
    meck:unload(redis),
    Config.

node_register_test_valid_node(Config) ->
    {IPaddress, Port, PublicKey, ValidNodeId} = ?config(validnode, Config),
    ValidNodeSecretHash = "Valide",
    {ValidNodeId, ValidNodeSecretHash} = node_service:node_register(IPaddress, Port, PublicKey),
    true = test_helpers:check_function_called(node_graph_manager, add_node, [IPaddress, Port, PublicKey]),
    true = test_helpers:check_function_called(heartbeat_monitor, add_node, [ValidNodeId]).

node_register_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {_InvIPaddress, InvPort, InvPublicKey, _} = ?config(invalidnode, Config),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, InvPort,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, -1,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, 65537,PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_register_test_invalid_ip(Config) ->
    {_IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    InvalidIp1 = "192.168.10.10.10",
    InvalidIp2 = "192",
    InvalidIp3 = "257.257.257.257",
    InvalidIp4 = "hallowereld",
    InvalidIp5 = 42,  
    test_helpers:assert_fail(fun node_service:node_register/3, [InvalidIp1, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvalidIp2, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvalidIp3, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvalidIp4, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvalidIp5, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_register_test_double_registration(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    meck:expect(node_graph_manager, add_node, fun(_,_,_) -> error(node_already_exists) end),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, Port, PublicKey], error, node_already_exists, failed_to_catch_invalid_argument),
    true = test_helpers:check_function_called(node_graph_manager, add_node, [IPaddress, Port, PublicKey]),
    false = test_helpers:check_function_called(heartbeat_monitor, add_node, [IPaddress ++ ":" ++ Port]).

node_unregister_test_valid_node(Config) ->
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    ok = node_service:node_unregister(NodeId, SecretHash),
    ok = node_service:node_unregister(NodeId),
    true = test_helpers:check_function_called(node_graph_manager, get_node_secret_hash, [NodeId]),
    true = test_helpers:check_function_called(node_graph_manager, remove_node, [NodeId]),
    true = test_helpers:check_function_called(heartbeat_monitor, remove_node, [NodeId]).

node_unregister_test_invalid_node(Config) ->
    {NodeId, SecretHash} = ?config(invalidnodeverify1, Config),
    {NodeId2, SecretHash2} = ?config(invalidnodeverify2, Config),
    {NodeId3, SecretHash3} = ?config(invalidnodeverify3, Config),
    test_helpers:assert_fail(fun node_service:node_unregister/2, [NodeId, SecretHash], error, {badmatch, "Testing123#"}, failed_to_catch_invalid_hash),
    test_helpers:assert_fail(fun node_service:node_unregister/2, [NodeId2, SecretHash2], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_unregister/2, [NodeId3, SecretHash3], error, nodenotfound, failed_to_catch_invalid_nodeid),
    test_helpers:assert_fail(fun node_service:node_unregister/1, [NodeId2], error, function_clause, failed_to_catch_invalid_argument).

node_verify_test_valid_node(Config) ->
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    SecretHash = node_service:node_verify(NodeId, SecretHash),
    true = test_helpers:check_function_called(node_graph_manager, get_node_secret_hash, [NodeId]).

node_verify_test_invalid_node(Config) ->
    {NodeId, SecretHash} = ?config(invalidnodeverify1, Config),
    {NodeId2, SecretHash2} = ?config(invalidnodeverify2, Config),
    {NodeId3, SecretHash3} = ?config(invalidnodeverify3, Config),
    test_helpers:assert_fail(fun node_service:node_verify/2, [NodeId, SecretHash], error, {badmatch, "Testing123#"}, failed_to_catch_invalid_hash),
    test_helpers:assert_fail(fun node_service:node_verify/2, [NodeId2, SecretHash2], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_verify/2, [NodeId3, SecretHash3], error, nodenotfound, failed_to_catch_invalid_nodeid).

node_update_test_valid_node(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    node_service:node_update(NodeId, SecretHash, IPaddress, Port, PublicKey),
    true = test_helpers:check_function_called(node_graph_manager, get_node_secret_hash, [NodeId]).

node_update_test_undefined_values_failure_test(Config) ->
    {_IPaddress, _Port, PublicKey, _} = ?config(validnode, Config),
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId, SecretHash, undefined, undefined, PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_update_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {_InvIPaddress, InvPort, InvPublicKey, _} = ?config(invalidnode, Config),
    {ValidNodeId, ValidHash} = ?config(validnodeverify, Config),
    {NodeId, SecretHash} = ?config(invalidnodeverify1, Config),
    {NodeId2, SecretHash2} = ?config(invalidnodeverify2, Config),
    {NodeId3, SecretHash3} = ?config(invalidnodeverify3, Config),
    %% Verify the secret hash part of the function
    ExpectedBadMatch = {badmatch, "Testing123#"},
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId, SecretHash, IPaddress, Port, PublicKey], error, ExpectedBadMatch, failed_to_catch_invalid_hash),
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId2, SecretHash2, IPaddress, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId3, SecretHash3, IPaddress, Port, PublicKey], error, nodenotfound, failed_to_catch_invalid_nodeid),
    %% Verify the port and publickey part of the function
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, InvPort,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, -1,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, 65537,PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_update_test_new_ip(Config) ->
    {_IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    NewIPaddress = "255.255.255.255",
    NewNodeId = "255.255.255.255:" ++ integer_to_list(Port),
    meck:expect(node_graph_manager, add_node, fun(_,_,_) ->
        {NewNodeId, SecretHash} end),

    node_service:node_update(NodeId, SecretHash, NewIPaddress, Port, PublicKey),
    true = test_helpers:check_function_called(node_graph_manager, remove_node, [NodeId]),
    true = test_helpers:check_function_called(node_graph_manager, get_node_secret_hash, [NodeId]),
    true = test_helpers:check_function_called(node_graph_manager, add_node, [NewIPaddress, Port, PublicKey]),
    true = test_helpers:check_function_called(heartbeat_monitor, add_node, [NewNodeId]),
    true = test_helpers:check_function_called(heartbeat_monitor, remove_node, [NodeId]),
    true = test_helpers:check_function_called(redis, set, ["node_hash_" ++ NewNodeId, SecretHash]),
    true = test_helpers:check_function_called(redis, get, ["node_edges_" ++ NodeId]).

node_update_test_invalid_ip(Config) ->
    {_IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {ValidNodeId, ValidHash} = ?config(validnodeverify, Config),
    InvalidIp1 = "192.168.10.10.10",
    InvalidIp2 = "192",
    InvalidIp3 = "257.257.257.257",
    InvalidIp4 = "hallowereld",
    InvalidIp5 = 42,
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvalidIp1, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvalidIp2, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvalidIp3, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvalidIp4, Port, PublicKey], error, einval, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvalidIp5, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument).