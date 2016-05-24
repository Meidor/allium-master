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
    log_in_admin_from_ws_client_return_succes_test/1,
    log_in_invalid_admin_from_ws_client_return_error_test/1,
    register_node_in_backend_receive_graph_update_test/1,
    update_node_in_backend_receive_graph_update_test/1,
    delete_node_in_backend_receive_graph_update_test/1,
    make_request_without_being_logged_in_return_nothing_test/1,
    update_node_from_ws_client_test/1,
    update_one_ws_client_sent_to_all_clients_test/1,
    update_backend_sent_to_all_clients_test/1,
    register_admin_while_logged_in_as_super_admin_return_all_admins_test/1,
    register_admin_while_logged_in_as_normal_admin_return_nothing_test/1,
    register_admin_while_not_logged_return_nothing_test/1,
    update_admin_while_logged_in_as_normal_admin_return_nothing_test/1,
    update_admin_while_logged_in_as_super_admin_return_all_admins_test/1,
    update_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test/1,
    request_all_admins_while_logged_in_as_normal_admin_return_nothing_test/1,
    request_all_admins_while_logged_in_as_super_admin_return_all_admins_test/1,
    delete_admin_while_logged_in_as_normal_admin_return_nothing_test/1,
    delete_admin_while_logged_in_as_super_admin_return_all_admins_test/1,
    delete_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test/1
]).

all() -> [
    connect_to_websocket_return_full_graph_test,
    log_in_admin_from_ws_client_return_succes_test,
    log_in_invalid_admin_from_ws_client_return_error_test,
    register_node_in_backend_receive_graph_update_test,
    update_node_in_backend_receive_graph_update_test,
    delete_node_in_backend_receive_graph_update_test,
    make_request_without_being_logged_in_return_nothing_test,
    update_node_from_ws_client_test,
    update_one_ws_client_sent_to_all_clients_test,
    update_backend_sent_to_all_clients_test,
    register_admin_while_logged_in_as_super_admin_return_all_admins_test,
    register_admin_while_logged_in_as_normal_admin_return_nothing_test,
    register_admin_while_not_logged_return_nothing_test,
    update_admin_while_logged_in_as_normal_admin_return_nothing_test,
    update_admin_while_logged_in_as_super_admin_return_all_admins_test,
    update_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test,
    request_all_admins_while_logged_in_as_normal_admin_return_nothing_test,
    request_all_admins_while_logged_in_as_super_admin_return_all_admins_test,
    delete_admin_while_logged_in_as_normal_admin_return_nothing_test,
    delete_admin_while_logged_in_as_super_admin_return_all_admins_test,
    delete_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test
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
    persistence_service:update_admin("SuperAdmin", SuperPassword, true),
    Pid = handle_start_up(),
    [
        {pid, Pid},
        {regularadmin, {"Admin", Password}},
        {superadmin, {"SuperAdmin", SuperPassword}}
    ] ++ Config.

end_per_suite(Config) ->
    Config.

connect_to_websocket_return_full_graph_test(Config) ->
    EmptyGraph = ?config(emptygraph, Config),
    {ok, Pid} = ws_client_handler:start_link(),

    verify_received_messages([Pid], [EmptyGraph]).

log_in_admin_from_ws_client_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(regularadmin, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, Password}}
    ]),
    LoginResponse = {adminloginresponse, 'SUCCES'},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [LoginResponse]).

log_in_invalid_admin_from_ws_client_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, "InvalidUsername", "password"}}
    ]),
    LoginResponse = {adminloginresponse, 'FAILED'},

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

update_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid]),

    ok = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, UpdatedPublicKey),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

delete_node_in_backend_receive_graph_update_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid]),

    GraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ok = node_service:node_unregister(NodeId, SecretHash),

    verify_received_messages([Pid], [GraphUpdate]).

make_request_without_being_logged_in_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid]),

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [noresponse]).

update_node_from_ws_client_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid]),

    Msg = hrp_pb:encode([
            {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
    ]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

update_one_ws_client_sent_to_all_clients_test(Config) ->
    Pid = ?config(pid, Config),
    Pid2 = handle_start_up(),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),
    login_as_admin(Pid2, ?config(regularadmin, Config)),
    {NodeId, _SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid, Pid2]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
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

    {NodeId, SecretHash} = register_node({IP, Port, PublicKey}, 1, [Pid, Pid2]),

    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
    {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
    {graphupdate, 4, false, [{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    ok = node_service:node_update(NodeId, SecretHash, UpdatedIP, UpdatedPort, UpdatedPublicKey),
    verify_received_messages([Pid2, Pid], [SecondGraphUpdate, ThirdGraphUpdate]).

register_admin_while_not_logged_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

register_admin_while_logged_in_as_normal_admin_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

register_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINREGISTERREQUEST', {adminregisterrequest, "Username"}}
    ]),

    AdminListResponse = {adminlistresponse,
        [{admin, "Admin", false}, {admin, "Username", false}, {admin, "SuperAdmin", true}]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [AdminListResponse]).

update_admin_while_logged_in_as_normal_admin_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", true}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

update_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(superadmin, Config),
    {RegularUsername, _Password} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    AdminListResponse = {adminlistresponse,
        [{admin, "Admin", true},{admin, "SuperAdmin", true}]},

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, RegularUsername, "OtherPassword", true}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [AdminListResponse]).

update_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINUPDATEREQUEST', {adminupdaterequest, Username, "OtherPassword", false}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

request_all_admins_while_logged_in_as_normal_admin_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(regularadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLISTREQUEST', {adminlistrequest}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

request_all_admins_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid, ?config(superadmin, Config)),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLISTREQUEST', {adminlistrequest}}
    ]),

    AdminListResponse = {adminlistresponse,
        [{admin, "Admin", false},{admin, "SuperAdmin", true}]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [AdminListResponse]).

delete_admin_while_logged_in_as_normal_admin_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, Username}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

delete_admin_while_logged_in_as_super_admin_return_all_admins_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(superadmin, Config),
    {RegularUsername, _Password} = ?config(regularadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, RegularUsername}}
    ]),

    AdminListResponse = {adminlistresponse,[{admin, Username, true}]},

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], [AdminListResponse]).

delete_only_superadmin_to_regular_admin_while_logged_in_return_nothing_test(Config) ->
    Pid = ?config(pid, Config),
    {Username, Password} = ?config(superadmin, Config),
    login_as_admin(Pid, {Username, Password}),

    Msg = hrp_pb:encode([
        {wrapper, 'ADMINDELETEREQUEST', {admindeleterequest, Username}}
    ]),

    ws_client_handler:send(Pid, Msg),
    verify_received_messages([Pid], []).

-spec verify_received_messages(list(), list()) -> any().
verify_received_messages(Threads, Messages) ->
    [Msg = ws_client_handler:recv(Pid) || Msg <- lists:append(Messages, [noresponse]), Pid <- Threads].

-spec handle_start_up() -> any().
handle_start_up() ->
    {ok, Pid} = ws_client_handler:start_link(),
    GraphUpdates = iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}
    )),
    {graphupdateresponse, [GraphUpdates]} = ws_client_handler:recv(Pid),
    Pid.

-spec login_as_admin(pid(), tuple()) -> any().
login_as_admin(Pid, {Username, Password}) ->
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, Username, Password}}]),
    ws_client_handler:send(Pid, Msg),
    {adminloginresponse, 'SUCCES'} = ws_client_handler:recv(Pid).

-spec register_node(tuple(), integer(), list()) -> any().
register_node({IP, Port, PublicKey}, GraphUpdateNr, ThreadsListening) ->
    {NodeId, SecretHash} = node_service:node_register(IP, Port, PublicKey),
    GraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, GraphUpdateNr + 1, false, [{node, NodeId, IP, Port, PublicKey, []}], []}))]},
    verify_received_messages(ThreadsListening, [GraphUpdate]),
    {NodeId, SecretHash}.