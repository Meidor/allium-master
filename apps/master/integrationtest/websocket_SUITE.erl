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
    make_request_without_being_logged_in_return_error_test/1,
    update_node_from_ws_client_test/1,
    log_in_admin_from_ws_client_return_succes_test/1
]).

all() -> [
    connect_to_websocket_return_full_graph_test,
    make_request_without_being_logged_in_return_error_test,
    update_node_from_ws_client_test,
    log_in_admin_from_ws_client_return_succes_test
].

init_per_suite(Config) ->
    ok = application:load(websocket),
    ok = application:load(master),
    persistence_service:init(),
    application:ensure_all_started(websocket),
    Config.

init_per_testcase(connect_to_websocket_return_full_graph, Config) ->
%%    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    Config;
init_per_testcase(_, Config) ->
%%    persistence_service:delete_all_admins(),
    test_helpers_int:empty_database(),
    Pid = handle_start_up(),
    [
        {node, {"192.168.1.2", 80, <<"PublicKey">>}},
        {updatednode, {"192.123.1.1", 50, <<"OtherKey">>}},
        {pid, Pid}
    ] ++ Config.

end_per_suite(Config) ->
    Config.

connect_to_websocket_return_full_graph_test(_Config) ->
    {ok, Pid} = ws_client_handler:start_link(),
    GraphUpdates = iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}
    )),
    {graphupdateresponse, [GraphUpdates]} = ws_client_handler:recv(Pid).

make_request_without_being_logged_in_return_error_test(Config) ->
    Pid = ?config(pid, Config),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = node_service:node_register(IP, Port, PublicKey),

    ws_client_handler:recv(Pid),

    Msg = hrp_pb:encode([
        {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
    ]),

    ws_client_handler:send(Pid, Msg),
    {error, noresponse} = ws_client_handler:recv(Pid).

update_node_from_ws_client_test(Config) ->
    Pid = ?config(pid, Config),
    login_as_admin(Pid),
    {IP, Port, PublicKey} = ?config(node, Config),
    {UpdatedIP, UpdatedPort, UpdatedPublicKey} = ?config(updatednode, Config),
    {NodeId, _SecretHash} = node_service:node_register(IP, Port, PublicKey),

    FirstGraphUpdate = ws_client_handler:recv(Pid),

    Msg = hrp_pb:encode([
            {wrapper, 'UPDATENODE', {updatenode,{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}}}
    ]),

    FirstGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 2, false, [{node, NodeId, IP, Port, PublicKey, []}], []}))]},
    SecondGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 3, false, [], [{node, NodeId , [], 0, <<>>, []}]}))]},
    ThirdGraphUpdate = {graphupdateresponse, [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 4, false, [{node, NodeId, UpdatedIP, UpdatedPort, UpdatedPublicKey, []}] ,[]}))]},

    ws_client_handler:send(Pid, Msg),
    SecondGraphUpdate = ws_client_handler:recv(Pid),
    ThirdGraphUpdate = ws_client_handler:recv(Pid).

log_in_admin_from_ws_client_return_succes_test(Config) ->
    Pid = ?config(pid, Config),
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, "Username", "password"}}
    ]),
    ws_client_handler:send(Pid, Msg),
    logged_in = ws_client_handler:recv(Pid).

handle_start_up() ->
    {ok, Pid} = ws_client_handler:start_link(),
    GraphUpdates = iolist_to_binary(hrp_pb:encode(
        {graphupdate, 1, true, [], []}
    )),
    {graphupdateresponse, [GraphUpdates]} = ws_client_handler:recv(Pid),
    Pid.

login_as_admin(Pid) ->
    Msg = hrp_pb:encode([
        {wrapper, 'ADMINLOGINREQUEST', {adminloginrequest, "Admin", "Password"}}]),
    ws_client_handler:send(Pid, Msg),
    logged_in = ws_client_handler:recv(Pid).