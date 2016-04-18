-module(node_service_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([node_register_test_valid_node/1, node_register_test_invalid_node/1, node_unregister_test/1]).

all() -> [node_register_test_valid_node, node_unregister_test, node_register_test_invalid_node].

init_per_testcase(_, Config) ->
    IPaddress = "192.168.4.4",
    Port = 1337,
    PublicKey = "MyPublicKey",
    ValidNode = {IPaddress, Port, PublicKey},
    InvalidNode = {IPaddress, "Harry", PublicKey},
    [{validnode,ValidNode},  {invalidnode, InvalidNode}] ++ Config.

end_per_testcase(_, Config) ->
    Config.

node_register_test_valid_node(Config) -> 
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    meck:new(node_graph_manager, [non_strict, passthrough]),
    meck:expect(node_graph_manager, add_node, fun(IPaddress, Port, PublicKey) -> ok end),
    meck:new(heartbeat_monitor, [non_strict, passthrough]),
    meck:expect(heartbeat_monitor, add_node, fun(IPaddress, Port, PublicKey) -> ok end),
    ok = node_service:node_register(IPaddress, Port, PublicKey).

node_register_test_invalid_node(Config) ->
    {IPaddress, Port, PublicKey} = ?config(invalidnode, Config),
    meck:new(node_graph_manager, [non_strict, passthrough]),
    meck:expect(node_graph_manager, add_node, fun(IPaddress, Port, PublicKey) -> ok end),
    meck:new(heartbeat_monitor, [non_strict, passthrough]),
    meck:expect(heartbeat_monitor, add_node, fun(IPaddress, Port, PublicKey) -> ok end),
    test_helpers:assert_fail(fun node_service:node_register/3, [IPaddress, Port,PublicKey], error, function_clause, failed_to_catch_invalid_argument).

node_unregister_test(Config) ->
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    node_service:node_unregister(IPaddress, Port, PublicKey).