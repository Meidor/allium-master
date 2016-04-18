-module(node_service_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([node_register_test_valid_node/1, node_register_test_invalid_node/1, node_unregister_test_valid_node/1, node_unregister_test_invalid_node/1]).

all() -> [node_register_test_valid_node, node_unregister_test_valid_node, 
          node_register_test_invalid_node, node_unregister_test_invalid_node].

init_per_testcase(_, Config) ->
    IPaddress = "192.168.4.4",
    Port = 1337,
    PublicKey = "MyPublicKey",
    ValidNode = {IPaddress, Port, PublicKey},
    InvalidNode = {42, "Harry", 42},
    meck:new(node_graph_manager, [non_strict, passthrough]),
    meck:expect(node_graph_manager, add_node, fun(_,_,_) -> ok end),
    meck:expect(node_graph_manager, remove_node, fun(_,_,_) -> ok end),
    meck:new(heartbeat_monitor, [non_strict, passthrough]),
    meck:expect(heartbeat_monitor, add_node, fun(_,_,_) -> ok end),
    meck:expect(heartbeat_monitor, remove_node, fun(_,_,_) -> ok end),
    [{validnode,ValidNode},  {invalidnode, InvalidNode}] ++ Config.

end_per_testcase(_, Config) ->
    meck:unload(node_graph_manager),
    meck:unload(heartbeat_monitor),
    Config.

node_register_test_valid_node(Config) -> 
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    node_service:node_register(IPaddress, Port, PublicKey).

node_register_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    {InvIPaddress, InvPort, InvPublicKey} = ?config(invalidnode, Config),
    test_helpers:assert_fail(fun node_service:node_register/3, [InvIPaddress, Port, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, InvPort,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, -1,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, 65537,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_unregister_test_valid_node(Config) ->
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    node_service:node_unregister(IPaddress, Port, PublicKey).

node_unregister_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    {InvIPaddress, InvPort, InvPublicKey} = ?config(invalidnode, Config),
    test_helpers:assert_fail(fun node_service:node_unregister/3, [InvIPaddress, Port,PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_unregister/3, [IPaddress, InvPort, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_unregister/3, [IPaddress, -1, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_unregister/3, [IPaddress, 65537, PublicKey], error, function_clause, failed_to_catch_invalid_argument),
    test_helpers:assert_fail(fun node_service:node_unregister/3, [IPaddress, Port, InvPublicKey], error, function_clause, failed_to_catch_invalid_argument).