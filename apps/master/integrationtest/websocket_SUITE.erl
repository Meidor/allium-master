-module(websocket_SUITE).

-include_lib("common_test/include/ct.hrl").

-define(MINIMUMLENGTHPASSWORD, 5).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2
]).

-export([
    connect_to_websocket_return_empty_graph_when_no_existing_nodes_test/1,
    connect_to_websocket_return_full_graph_when_existing_nodes_test/1,
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
    reset_password_change_status_admin_return_with_generated_password_no_changed_status_and_other_admins_test/1,
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
    connect_to_websocket_return_empty_graph_when_no_existing_nodes_test,
    connect_to_websocket_return_full_graph_when_existing_nodes_test,
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
    reset_password_change_status_admin_return_with_generated_password_no_changed_status_and_other_admins_test,
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
    application:load(websocket),
    application:load(master),
    application:ensure_all_started(websocket),
    persistence_service:init(),
    test_helpers_int:init_sharded_eredis(),
    EmptyGraph = {graphupdateresponse,[iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}))]},
    [
        {node, {"192.168.1.2", 80, <<"PublicKey">>}},
        {nodewithedges, {"192.168.1.2", 80, <<"PublicKey">>, [{edge, "Node1", 10.0}, {edge, "Node2", 15.0}]}},
        {updatednode, {"192.123.1.1", 50, <<"OtherKey">>, [{edge, "OtherNode", 1.0}, {edge, "Node2", 5.0}]}},
        {emptygraph, EmptyGraph}
    ] ++ Config.

init_per_testcase(connect_to_websocket_return_full_graph, Config) ->
    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    Config;
init_per_testcase(_, Config) ->
    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    {Admin, SuperAdmin} = {"Admin", "SuperAdmin"},
    persistence_service:insert_admin(Admin),
    {Admin, Password, false} = persistence_service:select_admin(Admin),
    persistence_service:insert_admin(SuperAdmin),
    {SuperAdmin, SuperPassword, false} = persistence_service:select_admin(SuperAdmin),
    persistence_service:update_admin(SuperAdmin, SuperPassword, true, false),
    Pid = handle_start_up(),
    [
        {admins, {Admin, SuperAdmin}},
        {pid, Pid},
        {regularadmin, {Admin, Password, false}},
        {superadmin, {SuperAdmin, SuperPassword, true}}
    ] ++ Config.

end_per_suite(Config) ->
    application:unload(master),
    application:unload(websocket),
    Config.

connect_to_websocket_return_empty_graph_when_no_existing_nodes_test(Config) ->
    EmptyGraph = ?config(emptygraph, Config),
    {ok, Pid} = ws_client_handler:start_link(),

    verify_received_messages([Pid], [EmptyGraph]).

connect_to_websocket_return_full_graph_when_existing_nodes_test(Config) ->
    {IP, Port, PublicKey} = ?config(node, Config),
    {OtherIP, OtherPort, OtherPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = node_service:node_register(IP, Port, PublicKey),
    {OtherNodeId, _OtherSecretHash} = node_service:node_register(OtherIP, OtherPort, OtherPublicKey),

    FullGraph =  {graphupdateresponse,[
        iolist_to_binary(hrp_pb:encode(
            {graphupdate, 1, true, [], []})),
        iolist_to_binary(hrp_pb:encode(
            {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []})),
        iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [{node, OtherNodeId, OtherIP, OtherPort, OtherPublicKey, []}], []}))
        ]},
    {ok, Pid} = ws_client_handler:start_link(),

    verify_received_messages([Pid], [FullGraph]).

log_in_regular_admin_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, false} = ?config(regularadmin, Config),
    Msg = {adminloginrequest, Username, Password},
    Response = {adminloginresponse, 'SUCCES', false},

    ws_client_handler:send(Pid, 'ADMINLOGINREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

log_in_super_admin_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, true} = ?config(superadmin, Config),
    Msg = {adminloginrequest, Username, Password},
    Response = {adminloginresponse, 'SUCCES', true},

    ws_client_handler:send(Pid, 'ADMINLOGINREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

log_in_invalid_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    Msg = {adminloginrequest, "InvalidUsername", "password"},
    Response = {adminloginresponse, 'FAILED', false},

    ws_client_handler:send(Pid, 'ADMINLOGINREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

log_in_admin_with_wrong_password_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, false} = ?config(regularadmin, Config),
    Msg = {adminloginrequest, Username, "OtherPassword"},
    Response = {adminloginresponse, 'FAILED', false},

    ws_client_handler:send(Pid, 'ADMINLOGINREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

register_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = node_service:node_register(IP, Port, PublicKey),

    Response = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []}))]},

    verify_received_messages([Pid], [Response]).

update_node_ip_port_from_backend_edges_removed_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {UpdatedIP, UpdatedPort, _UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    NewNodeId = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, PublicKey),
    NodeDeleteResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    NodeAddResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NewNodeId, UpdatedIP, UpdatedPort, PublicKey, []}] ,[]}))]},

    verify_received_messages([Pid], [NodeDeleteResponse, NodeAddResponse]).

update_node_public_key_from_backend_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    NodeDeleteResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    NodeAddResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, UpdatedPublicKey, Edges}] ,[]}))]},
    NodeId = node_service:node_update(NodeId, SecretHash, IP, Port, UpdatedPublicKey),

    verify_received_messages([Pid], [NodeDeleteResponse, NodeAddResponse]).

delete_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid]),

    Response = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]
    },
    node_service:node_unregister(NodeId, SecretHash),

    verify_received_messages([Pid], [Response]).

make_request_without_being_logged_in_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid]),

    Msg = {updatenode, {node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid], [noresponse]).

update_node_ip_port_from_ws_client_edges_removed_receive_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    Msg = {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, Edges}},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid], [noresponse]).

update_node_public_key_from_ws_client_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    Msg = {updatenode,{node, NodeId, IP, Port, UpdatedPublicKey, Edges}},
    DeleteNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    AddNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, UpdatedPublicKey, Edges}] ,[]}))]},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid], [DeleteNodeResponse, AddNodeResponse]).

update_node_edges_from_ws_client_edges_remain_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey, Edges} = ?config(nodewithedges, Config),
    {_UpdatedIP, _UpdatedPort, _UpdatedPublicKey, UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, Edges}, 1, [Pid]),

    Msg = {updatenode,{node, NodeId, IP, Port, PublicKey, UpdatedEdges}},
    DeleteNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    AddNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, PublicKey, UpdatedEdges}] ,[]}))]},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid], [DeleteNodeResponse, AddNodeResponse]).

update_one_ws_client_sent_to_all_clients_test(Config) ->
    Pid = ?config(pid, Config),
    Pid2 = handle_start_up(),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    login_as_admin(Pid2, ?config(regularadmin, Config)),
    {_UpdatedIP, _UpdatedPort, _UpdatedPublicKey, UpdatedEdges} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey, UpdatedEdges}, 1, [Pid, Pid2]),

    Msg = {updatenode,{node, NodeId, IP, Port, PublicKey, UpdatedEdges}},
    DeleteNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 5, false, [], [{node, NodeId, [], 0, <<>>, []}]}))]},
    AddNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 6, false, [{node, NodeId, IP, Port, PublicKey, UpdatedEdges}] ,[]}))]},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid2, Pid], [DeleteNodeResponse, AddNodeResponse]).

update_backend_sent_to_all_clients_test(Config) ->
    Pid = ?config(pid, Config),
    Pid2 = handle_start_up(),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey, _UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    login_as_admin(Pid2, ?config(superadmin, Config)),

    {NodeId, SecretHash} = register_node({IP, Port, PublicKey, []}, 1, [Pid, Pid2]),
    NewNodeId = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, UpdatedPublicKey),

    DeleteNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    AddNodeResponse = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NewNodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    verify_received_messages([Pid2, Pid], [DeleteNodeResponse, AddNodeResponse]).

register_admin_while_not_logged_return_error_test(Config) ->
    Pid = ?config(pid, Config),

    Msg = {adminregisterrequest, "Username"},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINREGISTERREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

register_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminregisterrequest, "Username"},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINREGISTERREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

register_admin_existing_username_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminregisterrequest, Username},
    Response = {adminlistresponse, 'USERNAME_TAKEN', [], undefined},

    ws_client_handler:send(Pid, 'ADMINREGISTERREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

register_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Admin, SuperAdmin} = ?config(admins, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),

    Msg = {adminregisterrequest, "Username"},
    Admins = [{admin, Admin, false}, {admin, "Username", false}, {admin, SuperAdmin, true}],

    ws_client_handler:send(Pid, 'ADMINREGISTERREQUEST', Msg),
    verify_password_changed_message(Admins, "", Pid).

update_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, Username, "OtherPassword", true, false},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_admin_with_wrong_password_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, "pass", true, false},
    Response = {adminlistresponse, 'INVALID_PASSWORD', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, "OtherPassword", true, false},
    Response = {adminlistresponse, 'SUCCES',
        [{admin, "Admin", true},{admin, "SuperAdmin", true}], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_admin_with_invalid_password_as_super_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, "pass", false, false},
    Response = {adminlistresponse, 'INVALID_PASSWORD', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

reset_password_admin_return_with_generated_password_and_other_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, undefined, false, true},
    Admins = [{admin, RegularUsername, false}, {admin, Username, true}],

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_password_changed_message(Admins, RegularPassword, Pid).

reset_password_change_status_admin_return_with_generated_password_no_changed_status_and_other_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, undefined, true, true},
    Admins = [{admin, RegularUsername, false}, {admin, Username, true}],

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_password_changed_message(Admins, RegularPassword, Pid).

reset_password_admin_with_password_return_all_admins_with_generated_password_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, "Password1234", false, true},
    Admins = [{admin, RegularUsername, false}, {admin, Username, true}],

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_password_changed_message(Admins, RegularPassword, Pid).

update_only_superadmin_to_regular_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, Username, "OtherPassword", false, false},
    Response = {adminlistresponse, 'LAST_SUPERADMIN', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_only_superadmin_to_superadmin_other_password_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, Username, "OtherPassword", IsSuperAdmin, false},
    Response = {adminlistresponse, 'SUCCES',
        [{admin, RegularUsername, false},{admin, Username, true}], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = {adminlistrequest},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINLISTREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),
    {RegularUsername, Username} = ?config(admins, Config),

    Msg = {adminlistrequest},
    Response = {adminlistresponse, 'SUCCES',
        [{admin, RegularUsername, false}, {admin, Username, true}], undefined},

    ws_client_handler:send(Pid, 'ADMINLISTREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_while_logged_in_as_normal_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {admindeleterequest, Username},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINDELETEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {admindeleterequest, RegularUsername},
    Response = {adminlistresponse, 'SUCCES', [{admin, Username, true}], undefined},

    ws_client_handler:send(Pid, 'ADMINDELETEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

delete_only_superadmin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {admindeleterequest, Username},
    Response = {adminlistresponse, 'LAST_SUPERADMIN', [], undefined},

    ws_client_handler:send(Pid, 'ADMINDELETEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_regular_admin_to_super_admin_with_password_undefined_password_remains_unchanged_return_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, undefined, true, false},
    Response = {adminlistresponse, 'SUCCES',
        [{admin, RegularUsername, true},{admin, Username, true}], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]),
    {RegularUsername, RegularPassword, true} = persistence_service:select_admin(RegularUsername).

update_regular_admin_to_super_admin_with_password_empty_password_remains_unchanged_return_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    {RegularUsername, RegularPassword, _IsSuperAdmin} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, RegularUsername, "", true, false},
    Response = {adminlistresponse, 'SUCCES',
        [{admin, RegularUsername, true},{admin, Username, true}], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]),
    {RegularUsername, RegularPassword, true} = persistence_service:select_admin(RegularUsername).

delete_non_existing_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {admindeleterequest, "NonExistingUsername"},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINDELETEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_non_existing_admin_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password, IsSuperAdmin} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password, IsSuperAdmin}),

    Msg = {adminupdaterequest, "NonExistingUsername", undefined, true, false},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

update_non_existing_node_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {_UpdatedIP, _UpdatedPort, UpdatedPublicKey, UpdatedEdges} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = {updatenode,{node, "192.11.1.1:40", "192.11.1.1", 40, UpdatedPublicKey, UpdatedEdges}},

    ws_client_handler:send(Pid, 'UPDATENODE', Msg),
    verify_received_messages([Pid], [noresponse]).

update_admin_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),

    Msg = {adminupdaterequest, Username, "OtherPassword", true, false},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINUPDATEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

delete_admin_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, _Password, _IsSuperAdmin} = ?config(regularadmin, Config),

    Msg = {admindeleterequest, Username},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINDELETEREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

request_all_admins_while_not_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),

    Msg = {adminlistrequest},
    Response = {adminlistresponse, 'FAILED', [], undefined},

    ws_client_handler:send(Pid, 'ADMINLISTREQUEST', Msg),
    verify_received_messages([Pid], [Response]).

-spec verify_received_messages(list(), list()) -> any().
verify_received_messages(Threads, Messages) ->
    [Msg = ws_client_handler:recv(Pid) || Msg <- lists:append(Messages, [noresponse]), Pid <- Threads].

-spec verify_password_changed_message(list(), list(), pid()) -> any().
verify_password_changed_message(Admins, OldPassword, Pid) ->
    {adminlistresponse, 'SUCCES', Admins, NewPassword} = ws_client_handler:recv(Pid),
    true = ?MINIMUMLENGTHPASSWORD =< length(NewPassword),
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
    Msg = {adminloginrequest, Username, Password},
    ws_client_handler:send(Pid, 'ADMINLOGINREQUEST', Msg),
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