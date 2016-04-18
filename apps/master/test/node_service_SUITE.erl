-module(node_service_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1]).
-export([node_register_test_valid_node/1, node_register_test_invalid_node/1, 
    node_unregister_test_valid_node/1, node_unregister_test_invalid_node/1, 
    node_verify_test_valid_node/1, node_verify_test_invalid_node/1,
    node_update_test_valid_node/1, node_update_test_invalid_node/1]).

all() -> [node_register_test_valid_node, node_unregister_test_valid_node, 
          node_register_test_invalid_node, node_unregister_test_invalid_node,
          node_verify_test_valid_node, node_verify_test_invalid_node,
          node_update_test_valid_node, node_update_test_invalid_node].

init_per_suite(Config) ->
    IPaddress = "192.168.4.4",
    Port = 1337,
    PublicKey = "MyPublicKey",
    ValidNode = {IPaddress, Port, PublicKey, "ValideId"},
    InvalidNode = {42, "Harry", 42, "InvalideId"},
    [
        {validnode,ValidNode},  
        {invalidnode, InvalidNode}, 
        {validnodeverify, {"ValideNode", "Valide"}}, 
        {invalidnodeverify1, {"InvalideNode", "Invalide"}},
        {invalidnodeverify2, {42, invalide}},
        {invalidnodeverify3, {"NodeNotFound", "NotFound"}}
    ] ++ Config.
   
init_per_testcase(_, Config) ->
    %% Setup Graph Manager Mock
    meck:new(node_graph_manager, [non_strict]),
    meck:expect(node_graph_manager, add_node, fun(_,_,_) -> 
        {"ValideNode", "Valide"} end),
    meck:expect(node_graph_manager, remove_node, fun(NodeId) -> 
        case NodeId of
            "ValideNode" -> ok;
            _ -> error(nodenotfound)
        end 
    end),
    meck:expect(node_graph_manager, get_node_secret_hash, fun(NodeId) -> 
        case NodeId of 
            "ValideNode" ->  "Valide";
            "InvalideNode" -> "Testing123#";
            _ -> error(nodenotfound) 
        end 
    end),
    meck:expect(node_graph_manager, update_node, fun(_,_,_,_) -> ok end),

    %% Setup Heartbeat Monitor Mock
    meck:new(heartbeat_monitor, [non_strict]),
    meck:expect(heartbeat_monitor, add_node, fun(_) -> ok end),
    meck:expect(heartbeat_monitor, remove_node, fun(NodeId) ->
        case NodeId of 
            "ValideNode" -> ok;
            _ -> error(nodenotfound) 
        end 
    end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(node_graph_manager),
    meck:unload(heartbeat_monitor),
    Config.

node_register_test_valid_node(Config) -> 
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {"ValideNode", "Valide"} = node_service:node_register(IPaddress, Port, PublicKey).

node_register_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {InvIPaddress, InvPort, InvPublicKey, _} = ?config(invalidnode, Config),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvIPaddress, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, InvPort,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, -1,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, 65537,PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_unregister_test_valid_node(Config) ->
    {NodeId, SecretHash} = ?config(validnodeverify, Config),
    node_service:node_unregister(NodeId, SecretHash),
    node_service:node_unregister(NodeId).

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
    node_service:node_verify(NodeId, SecretHash).

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
    node_service:node_update(NodeId, SecretHash, IPaddress, Port, PublicKey).

node_update_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey, _} = ?config(validnode, Config),
    {InvIPaddress, InvPort, InvPublicKey, _} = ?config(invalidnode, Config),
    {ValidNodeId, ValidHash} = ?config(validnodeverify, Config),
    {NodeId, SecretHash} = ?config(invalidnodeverify1, Config),
    {NodeId2, SecretHash2} = ?config(invalidnodeverify2, Config),
    {NodeId3, SecretHash3} = ?config(invalidnodeverify3, Config),
    %% Verify the secret hash part of the function
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId, SecretHash, IPaddress, Port, PublicKey], error, {badmatch, "Testing123#"}, failed_to_catch_invalid_hash),
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId2, SecretHash2, IPaddress, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [NodeId3, SecretHash3, IPaddress, Port, PublicKey], error, nodenotfound, failed_to_catch_invalid_nodeid),
    %% Verify the ip, port ,publickey part of the function
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, InvIPaddress, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, InvPort,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, -1,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_update/5, [ValidNodeId, ValidHash, IPaddress, 65537,PublicKey], error, function_clause, failed_to_catch_invalid_argument).