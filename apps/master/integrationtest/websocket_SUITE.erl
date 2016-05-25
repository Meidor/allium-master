-module(websocket_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2
]).

-export([
    connect_to_websocket_return_full_graph_test/1,
    log_in_regular_admin_return_succes_test/1,
    log_in_super_admin_return_succes_test/1,
    log_in_invalid_admin_return_error_test/1,
    log_in_admin_with_wrong_password_return_error_test/1,
    register_node_in_backend_receive_graph_update_test/1,
    update_node_ip_port_from_backend_edges_removed_receive_graph_update_test/1,
    update_node_public_key_from_backend_edges_remain_receive_graph_update_test/1,
    delete_node_in_backend_receive_graph_update_test/1,
    make_request_without_being_logged_in_return_nothing_test/1,
    update_node_ip_port_from_ws_client_edges_removed_receive_nothing_test/1,
    update_node_public_key_from_ws_client_edges_remain_receive_graph_update_test/1,
    update_node_edges_from_ws_client_edges_remain_receive_graph_update_test/1,
    update_one_ws_client_sent_to_all_clients_test/1,
    update_backend_sent_to_all_clients_test/1,
    register_admin_while_logged_in_as_super_admin_return_all_admins_test/1,
    register_admin_while_logged_in_as_normal_admin_return_error_test/1,
    register_admin_while_not_logged_return_error_test/1,
    register_admin_existing_username_return_error_test/1,
    update_admin_while_logged_in_as_normal_admin_return_error_test/1,
    update_admin_while_logged_in_as_super_admin_return_all_admins_test/1,
    update_admin_with_invalid_password_as_super_admin_return_error_test/1,
    update_only_superadmin_to_regular_admin_return_error_test/1,
    update_only_superadmin_to_superadmin_other_password_return_all_admins_test/1,
    request_all_admins_while_logged_in_as_normal_admin_return_error_test/1,
    request_all_admins_while_logged_in_as_super_admin_return_all_admins_test/1,
    delete_admin_while_logged_in_as_normal_admin_return_error_test/1,
    delete_admin_return_all_admins_test/1,
    delete_only_superadmin_return_error_test/1,
    reset_password_admin_with_password_return_all_admins_with_generated_password_test/1,
    reset_password_admin_return_with_generated_password_and_other_admins_test/1,
    update_admin_with_wrong_password_return_error_test/1,
    update_regular_admin_to_super_admin_with_password_undefined_password_remains_unchanged_return_admins_test/1,
    update_regular_admin_to_super_admin_with_password_empty_password_remains_unchanged_return_admins_test/1,
    delete_non_existing_admin_return_error_test/1,
    update_non_existing_admin_return_error_test/1,
    update_non_existing_node_return_nothing_test/1,
    update_admin_while_not_logged_in_return_error_test/1,
    delete_admin_while_not_logged_in_return_error_test/1,
    request_all_admins_while_not_logged_in_return_error_test/1
]).

all() -> [
    connect_to_websocket_return_full_graph_test,
    log_in_regular_admin_return_succes_test,
    log_in_super_admin_return_succes_test,
    log_in_invalid_admin_return_error_test,
    log_in_admin_with_wrong_password_return_error_test,
    register_node_in_backend_receive_graph_update_test,
    update_node_ip_port_from_backend_edges_removed_receive_graph_update_test,
    update_node_public_key_from_backend_edges_remain_receive_graph_update_test,
    delete_node_in_backend_receive_graph_update_test,
    make_request_without_being_logged_in_return_nothing_test,
    update_node_ip_port_from_ws_client_edges_removed_receive_nothing_test,
    update_node_public_key_from_ws_client_edges_remain_receive_graph_update_test,
    update_node_edges_from_ws_client_edges_remain_receive_graph_update_test,
    update_one_ws_client_sent_to_all_clients_test,
    update_backend_sent_to_all_clients_test,
    register_admin_while_logged_in_as_super_admin_return_all_admins_test,
    register_admin_while_logged_in_as_normal_admin_return_error_test,
    register_admin_while_not_logged_return_error_test,
    register_admin_existing_username_return_error_test,
    update_admin_while_logged_in_as_normal_admin_return_error_test,
    update_admin_while_logged_in_as_super_admin_return_all_admins_test,
    update_admin_with_invalid_password_as_super_admin_return_error_test,
    update_only_superadmin_to_regular_admin_return_error_test,
    update_only_superadmin_to_superadmin_other_password_return_all_admins_test,
    request_all_admins_while_logged_in_as_normal_admin_return_error_test,
    request_all_admins_while_logged_in_as_super_admin_return_all_admins_test,
    delete_admin_while_logged_in_as_normal_admin_return_error_test,
    delete_admin_return_all_admins_test,
    delete_only_superadmin_return_error_test,
    reset_password_admin_with_password_return_all_admins_with_generated_password_test,
    reset_password_admin_return_with_generated_password_and_other_admins_test,
    update_admin_with_wrong_password_return_error_test,
    update_regular_admin_to_super_admin_with_password_undefined_password_remains_unchanged_return_admins_test,
    update_regular_admin_to_super_admin_with_password_empty_password_remains_unchanged_return_admins_test,
    delete_non_existing_admin_return_error_test,
    update_non_existing_admin_return_error_test,
    update_non_existing_node_return_nothing_test,
    update_admin_while_not_logged_in_return_error_test,
    delete_admin_while_not_logged_in_return_error_test,
    request_all_admins_while_not_logged_in_return_error_test
].

init_per_suite(Config) ->
    ok = application:load(websocket),
    ok = application:load(master),
    persistence_service:init(),
    application:ensure_all_started(websocket),
    EmptyGraph = {graphupdateresponse,[iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}))]},
    [
        {node, {"192.168.1.2", 80, <<"PublicKey">>}},
        {nodewithedges, {"192.168.1.2", 80, <<"PublicKey">>, [{edge, "Node1", 10.0}, {edge, "Node2", 15.0}]}},
        {updatednode, {"192.123.1.1", 50, <<"OtherKey">>}},
        {emptygraph, EmptyGraph}
    ] ++ Config.

init_per_testcase(connect_to_websocket_return_full_graph, Config) ->
    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    Config;
init_per_testcase(_, Config) ->
    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    persistence_service:insert_admin("Admin"),
    {"Admin", Password, false} = persistence_service:select_admin("Admin"),
    persistence_service:insert_admin("SuperAdmin"),
    {"SuperAdmin", SuperPassword, false} = persistence_service:select_admin("SuperAdmin"),
    persistence_service:update_admin("SuperAdmin", SuperPassword, true, false),
    Pid = handle_start_up(),
    [
        {pid, Pid},
        {regularadmin, {"Admin", Password, false}},
        {superadmin, {"SuperAdmin", SuperPassword, true}}
    ] ++ Config.

end_per_suite(Config) ->
    Config.

connect_to_websocket_return_full_graph_test(Config) ->
    EmptyGraph = ?config(emptygraph, Config),
    {ok, Pid} = ws_client_handler:start_link(),

    verify_received_messages([Pid], [EmptyGraph]).

log_in_regular_admin_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, false} = ?config(regularadmin, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, Password}}
    ]),
    LoginResponse = {adminloginresponse, 'SUCCES', false},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [LoginResponse]).

log_in_super_admin_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, true} = ?config(superadmin, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, Password}}
    ]),
    LoginResponse = {adminloginresponse, 'SUCCES', true},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [LoginResponse]).

log_in_invalid_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, "InvalidUsername", "password"}}
    ]),
    LoginResponse = {adminloginresponse, 'FAILED', false},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [LoginResponse]).

log_in_admin_with_wrong_password_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, false} = ?config(regularadmin, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, "OtherPassword"}}
    ]),
    LoginResponse = {adminloginresponse, 'FAILED', false},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [LoginResponse]).

register_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = node_service:node_register(IP, Port, PublicKey),

    GraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []}))]},

    verify_received_messages([Pid], [GraphUpdate]).

update_node_ip_port_from_backend_edges_removed_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {UpdatedIP, UpdatedPort, _UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    NewNodeId = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, PublicKey),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NewNodeId, UpdatedIP, UpdatedPort, PublicKey, []}] ,[]}))]},

    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

update_node_public_key_from_backend_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    NodeId = node_service:node_update(NodeId, SecretHash, IP, Port, UpdatedPublicKey),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, UpdatedPublicKey, Edges}] ,[]}))]},

    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

delete_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid]),

    GraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ok = node_service:node_unregister(NodeId, SecretHash),

    verify_received_messages([Pid], [GraphUpdate]).

make_request_without_being_logged_in_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid]),

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [noresponse]).

update_node_ip_port_from_ws_client_edges_removed_receive_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    Msg = hrp_pb:encode([
            {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, Edges}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [noresponse]).

update_node_public_key_from_ws_client_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, IP, Port, UpdatedPublicKey, Edges}}}
    ]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, UpdatedPublicKey, Edges}] ,[]}))]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

update_node_edges_from_ws_client_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),
    UpdatedEdges = [{edge, "newNode", 1.0}, {edge, "NewNode2", 2.0}],

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, IP, Port, PublicKey, UpdatedEdges}}}
    ]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, PublicKey, UpdatedEdges}] ,[]}))]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

update_one_ws_client_sent_to_all_clients_test(Config) ->
    Pid = ?config(pid, Config),
    Pid2 = handle_start_up(),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    login_as_admin(Pid2, ?config(regularadmin, Config)),
    UpdatedEdges = [{edge, "newNode", 1.0}, {edge, "NewNode2", 2.0}],
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, UpdatedEdges}, 1, [Pid, Pid2]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId, [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, PublicKey, UpdatedEdges}] ,[]}))]},

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, IP, Port, PublicKey, UpdatedEdges}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid2, Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

update_backend_sent_to_all_clients_test(Config) ->
    Pid = ?config(pid, Config),
    Pid2 = handle_start_up(),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    login_as_admin(Pid2, ?config(regularadmin, Config)),

    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid, Pid2]),
    NewNodeId = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, UpdatedPublicKey),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NewNodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    verify_received_messages([Pid2, Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

register_admin_while_not_logged_return_error_test(Config) ->
    Pid = ?config(pid, Config),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

register_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

register_admin_existing_username_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, Username}}
    ]),

    Response = {adminlistresponse, 'USERNAME_TAKEN', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

register_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", false}, {admin, "Username", false}, {admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", true, false}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_admin_with_wrong_password_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "pass", true, false}}
    ]),

    Response = {adminlistresponse, 'INVALID_PASSWORD', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "OtherPassword", true, false}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", true},{admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_admin_with_invalid_password_as_super_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "pass", true, false}}
    ]),

    Response = {adminlistresponse, 'INVALID_PASSWORD', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

reset_password_admin_return_with_generated_password_and_other_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, undefined, true, true}}
    ]),
    Admins = [{admin, "Admin", true}, {admin, "SuperAdmin", true}],

    ws_client_handler:send(Pid, Msg),
    verify_password_changed_message(Admins, RegularPassword, Pid).

reset_password_admin_with_password_return_all_admins_with_generated_password_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "Password1234", true, true}}
    ]),
    Admins = [{admin, "Admin", true}, {admin, "SuperAdmin", true}],

    ws_client_handler:send(Pid, Msg),
    verify_password_changed_message(Admins, RegularPassword, Pid).

update_only_superadmin_to_regular_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", false, false}}
    ]),

    Response = {adminlistresponse, 'LAST_SUPERADMIN', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_only_superadmin_to_superadmin_other_password_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", IsSuperAdmin, false}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", false},{admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLISTREQUEST', {adminlistrequest}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLISTREQUEST', {adminlistrequest}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", false}, {admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, Username}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, RegularUsername}}
    ]),

    Response = {adminlistresponse, 'SUCCES', [{admin, Username, true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

delete_only_superadmin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, Username}}
    ]),

    Response = {adminlistresponse, 'LAST_SUPERADMIN', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_regular_admin_to_super_admin_with_password_undefined_password_remains_unchanged_return_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, undefined, true, false}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", true},{admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]),
    {RegularUsername, RegularPassword, true} = persistence_service:select_admin(RegularUsername).

update_regular_admin_to_super_admin_with_password_empty_password_remains_unchanged_return_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "", true, false}}
    ]),

    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", true},{admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]),
    {RegularUsername, RegularPassword, true} = persistence_service:select_admin(RegularUsername).

delete_non_existing_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, "NonExistingUsername"}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_non_existing_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, "NonExistingUsername", undefined, true, false}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

update_non_existing_node_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, "1.1.1.1:40", "1.1.1.1", 40, UpdatedPublicKey, []}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [noresponse]).

update_admin_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", true, false}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, Username}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLISTREQUEST', {adminlistrequest}}
    ]),

    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [Response]).

-spec verify_received_messages(list(), list()) -> any().
verify_received_messages(Threads, Messages) ->
    [Msg = ws_client_handler:recv(Pid) || Msg <- lists:append(Messages, [noresponse]), Pid <- Threads].

-spec verify_password_changed_message(list(), list(), pid()) -> any().
verify_password_changed_message(Admins, OldPassword, Pid) ->
    {adminlistresponse, 'SUCCES', Admins, NewPassword} = ws_client_handler:recv(Pid),
    true = 4 < length(NewPassword),
    false = OldPassword == NewPassword,
    noresponse = ws_client_handler:recv(Pid).

-spec handle_start_up() -> any().
handle_start_up() ->
    {ok, Pid} = ws_client_handler:start_link(),
    GraphUpdates = iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}
    )),
    {graphupdateresponse, [GraphUpdates]} = ws_client_handler:recv(Pid),
    Pid.

-spec login_as_admin(pid(), tuple()) -> any().
login_as_admin(Pid, {Username, Password, IsSuperAdmin}) ->
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, Password}}]),
    ws_client_handler:send(Pid, Msg),
    {adminloginresponse, 'SUCCES', IsSuperAdmin} = ws_client_handler:recv(Pid).

-spec register_node(tuple(), integer(), list()) -> any().
register_node({IP, Port, PublicKey, Edges}, GraphUpdateNr, ThreadsListening) ->
    {NodeId, SecretHash} = node_service:node_register(IP, Port, PublicKey),
    GraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, GraphUpdateNr + 1, false, [{node, NodeId, IP, Port, PublicKey, []}], []}))]},
    GraphDelete = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, GraphUpdateNr + 2, false, [], [{node, NodeId, "", 0, <<>>, []}]}))]},
    GraphAddition = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, GraphUpdateNr + 3, false, [{node, NodeId, IP, Port, PublicKey, Edges}], []}))]},

    verify_received_messages(ThreadsListening, [GraphUpdate]),
    case length(Edges) > 0 of
        true ->
            node_graph_manager:update_node(NodeId, IP, Port, PublicKey, Edges),
            verify_received_messages(ThreadsListening, [GraphDelete, GraphAddition]),
            {NodeId, SecretHash};
        false ->
            {NodeId, SecretHash}
    end.