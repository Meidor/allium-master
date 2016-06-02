-module(integration_client_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2
]).

-export([
    login_non_existing_client_return_error_test/1,
    register_client_return_success_message_test/1,
    register_client_with_invalid_username_return_error_message_test/1,
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test/1,
    login_client_while_logged_in_return_same_connected_nodes_test/1,
    get_clients_return_logged_in_clients_test/1,
    get_clients_while_not_logged_in_return_logged_in_clients_test/1,
    register_client_with_taken_username_return_error_test/1,
    logout_client_return_success_message_test/1,
    logout_non_existing_client_return_error_message_test/1,
    register_new_client_and_log_in_while_three_nodes_exist_return_all_nodes_as_dedicated_nodes_test/1,
    register_new_client_and_log_in_while_ten_nodes_exist_return_five_nodes_as_dedicated_nodes_test/1,
    register_new_client_and_log_in_twice_while_ten_nodes_exist_return_same_five_dedicated_nodes_different_hash_test/1,
    client_is_logged_out_after_not_sending_heartbeat_test/1,
    send_heartbeat_from_existing_client_return_ok_test/1,
    send_heartbeat_from_non_existing_client_return_error_test/1,
    request_client_group_return_only_clients_belonging_to_group_test/1,
    request_client_group_when_no_clients_with_group_return_empty_list_test/1
]).

all() -> [
    login_non_existing_client_return_error_test,
    register_client_return_success_message_test,
    register_client_with_invalid_username_return_error_message_test,
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test,
    login_client_while_logged_in_return_same_connected_nodes_test,
    get_clients_return_logged_in_clients_test,
    get_clients_while_not_logged_in_return_logged_in_clients_test,
    register_client_with_taken_username_return_error_test,
    logout_client_return_success_message_test,
    logout_non_existing_client_return_error_message_test,
    register_new_client_and_log_in_while_three_nodes_exist_return_all_nodes_as_dedicated_nodes_test,
    register_new_client_and_log_in_while_ten_nodes_exist_return_five_nodes_as_dedicated_nodes_test,
    register_new_client_and_log_in_twice_while_ten_nodes_exist_return_same_five_dedicated_nodes_different_hash_test,
    client_is_logged_out_after_not_sending_heartbeat_test,
    send_heartbeat_from_existing_client_return_ok_test,
    send_heartbeat_from_non_existing_client_return_error_test,
    request_client_group_return_only_clients_belonging_to_group_test,
    request_client_group_when_no_clients_with_group_return_empty_list_test
].

init_per_suite(Config) ->
    application:load(master),
    application:load(websocket),
    application:ensure_all_started(websocket),
    persistence_service:init(),
    test_helpers_int:init_sharded_eredis(),

    NodeIPsAndKeys = [
        {"255.255.0.1", <<"publickey1">>},
        {"255.255.0.2", <<"publickey2">>},
        {"255.255.0.3", <<"publickey3">>},
        {"255.255.0.4", <<"publickey4">>},
        {"255.255.0.5", <<"publickey5">>},
        {"255.255.0.6", <<"publickey6">>},
        {"255.255.0.7", <<"publickey7">>},
        {"255.255.0.8", <<"publickey8">>},
        {"255.255.0.9", <<"publickey9">>},
        {"255.255.0.10", <<"publickey10">>}
    ],
    ValidUsername = "username",
    ValidPassword = "Password1234",
    ValidClientPublicKey = <<"generatedpublickey">>,
    InvalidConnectedNodes = [],
    InvalidClientSecretHash = "secrethash6789",
    OtherUsername = "otherusername",
    OtherClientPublicKey = <<"otherpublickey">>,
    [
        {nodes, NodeIPsAndKeys},
        {client, {ValidUsername, ValidPassword, ValidClientPublicKey, InvalidClientSecretHash, InvalidConnectedNodes}},
        {otherclient, {OtherUsername, ValidPassword, OtherClientPublicKey}}
    ] ++ Config.

init_per_testcase(_, Config) ->
    test_helpers_int:empty_database(),
    persistence_service:delete_all_clients(),
    Config.

end_per_suite(Config) ->
    application:unload(master),
    application:unload(websocket),
    Config.

login_non_existing_client_return_error_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'INVALID_COMBINATION', undefined, []} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')).

register_client_return_success_message_test(Config) ->
    {Username, Password, _, _, _} = ?config(client, Config),
    Request = {clientregisterrequest, Username, Password},

    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')).

register_client_with_invalid_username_return_error_message_test(Config) ->
    {_, Password, _, _, _} = ?config(client, Config),
    Request = {clientregisterrequest, "$#(^$)$%^S", Password},

    {clientregisterresponse, 'FAILED'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')).

login_client_return_succes_message_with_hash_and_dedicated_nodes_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    [] = DedicatedNodes.

get_clients_return_logged_in_clients_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    OtherClients = ["OtherClient1", "OtherClient2", "OtherClient3", "OtherClient4"],
    [test_helpers_int:register_client(OtherUsername, Password) || OtherUsername <- OtherClients],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, _, []} = test_helpers_int:login_client(Username, Password, PublicKey),
    {"OtherClient3", Password, PublicKey, _, []} = test_helpers_int:login_client("OtherClient3", Password, PublicKey),
    Request = {clientrequest, 1},

    {clientresponse, [
        {client, "OtherClient3", PublicKey, []},
        {client, Username, PublicKey, []}
    ]} = hrp_pb:decode_clientresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREQUEST', 'CLIENTRESPONSE')).

get_clients_while_not_logged_in_return_logged_in_clients_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    OtherClients = ["OtherClient1", "OtherClient2", "OtherClient3", "OtherClient4"],
    [test_helpers_int:register_client(OtherUsername, Password) || OtherUsername <- OtherClients],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, _, []} = test_helpers_int:login_client(Username, Password, PublicKey),
    {"OtherClient3", Password, PublicKey, _, []} = test_helpers_int:login_client("OtherClient3", Password, PublicKey),
    Request = {clientrequest, 1},

    {clientresponse, [
        {client, "OtherClient3", PublicKey, []},
        {client, Username, PublicKey, []}
    ]} = hrp_pb:decode_clientresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREQUEST', 'CLIENTRESPONSE')).

register_client_with_taken_username_return_error_test(Config) ->
    {Username, Password, _, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    OtherPassword = "Differentpassword1234",
    Request = {clientregisterrequest, Username, OtherPassword},

    {clientregisterresponse, 'TAKEN_USERNAME'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')).

logout_client_return_success_message_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, []} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    Request = {clientlogoutrequest, Username, SecretHash},
    {clientlogoutresponse, 'SUCCES'} = hrp_pb:decode_clientlogoutresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGOUTREQUEST', 'CLIENTLOGOUTRESPONSE')),

    {Username, undefined, PublicKey, Password, []} =
        persistence_service:select_client(Username).

logout_non_existing_client_return_error_message_test(_Config) ->
    Request = {clientlogoutrequest, "NonExistingUsername", "SecretHash"},
    {clientlogoutresponse, 'FAILED'} = hrp_pb:decode_clientlogoutresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGOUTREQUEST', 'CLIENTLOGOUTRESPONSE')).

register_new_client_and_log_in_while_three_nodes_exist_return_all_nodes_as_dedicated_nodes_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    NodeIPsAndKeys = ?config(nodes, Config),
    NodeIds = [test_helpers_int:register_node_return_id(IP, Key) || {IP, Key} <- lists:sublist(NodeIPsAndKeys,3)],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    true = test_helpers_int:valid_secret_hash(SecretHash),
    3 = length(lists:filter(fun(NodeId) -> lists:member(NodeId, DedicatedNodes) end, NodeIds)).

register_new_client_and_log_in_while_ten_nodes_exist_return_five_nodes_as_dedicated_nodes_test(Config) ->
    Username = "anotherusername",
    Password = "Password1234",
    PublicKey = <<"anotherpublickey">>,
    NodeIPsAndKeys = ?config(nodes, Config),
    NodeIds = [test_helpers_int:register_node_return_id(IP, Key) || {IP, Key} <- NodeIPsAndKeys],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    true = test_helpers_int:valid_secret_hash(SecretHash),
    5 = length(lists:filter(fun(NodeId) -> lists:member(NodeId, DedicatedNodes) end, NodeIds)).

register_new_client_and_log_in_twice_while_ten_nodes_exist_return_same_five_dedicated_nodes_different_hash_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    NodeIPsAndKeys = ?config(nodes, Config),
    [test_helpers_int:register_node_return_id(IP, Key) || {IP, Key} <- NodeIPsAndKeys],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    {Username, Password, PublicKey, OtherSecretHash, DedicatedNodes} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    true = test_helpers_int:valid_secret_hash(OtherSecretHash),
    false = OtherSecretHash == SecretHash.

login_client_while_logged_in_return_same_connected_nodes_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    test_helpers_int:login_client(Username, Password, PublicKey),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    [] = DedicatedNodes.

client_is_logged_out_after_not_sending_heartbeat_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    {OtherUsername, OtherPassword, OtherPublicKey} = ?config(otherclient, Config),
    heartbeat_monitor_sup:start_link(),
    test_helpers_int:register_client(OtherUsername, OtherPassword),
    test_helpers_int:register_client(Username, Password),
    {OtherUsername, OtherPassword, OtherPublicKey, OtherSecretHash, []} =
        test_helpers_int:login_client(OtherUsername, OtherPassword, OtherPublicKey),
    {Username, Password, PublicKey, _SecretHash, []} =
    test_helpers_int:login_client(Username, Password, PublicKey),

    HeartbeatRequest = test_helpers_int:encode_message_to_binary({nodeheartbeat, OtherUsername, OtherSecretHash}),

    timer:sleep(4000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'CLIENTHEARTBEAT'),
    timer:sleep(4000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'CLIENTHEARTBEAT'),
    timer:sleep(4000),

    {Username, undefined, PublicKey, Password, []} = persistence_service:select_client(Username),
    {OtherUsername, OtherSecretHash, OtherPublicKey, OtherPassword, []} =
        persistence_service:select_client(OtherUsername).

send_heartbeat_from_existing_client_return_ok_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, []} =
        test_helpers_int:login_client(Username, Password, PublicKey),
    Request = {clientheartbeat, Username, SecretHash},

    Message = hrp_pb:encode(
        [{wrapper, 'CLIENTHEARTBEAT', test_helpers_int:encode_message_to_binary(Request)}]
    ),

    {ok,<<"OK">>} = master_app:handle_message(Message).

send_heartbeat_from_non_existing_client_return_error_test(_Config) ->
    Request = {clientheartbeat, "NonExistingUsername", "SecretHash"},

    Message = hrp_pb:encode(
        [{wrapper, 'CLIENTHEARTBEAT', test_helpers_int:encode_message_to_binary(Request)}]
    ),

    test_helpers:assert_fail(fun master_app:handle_message/1, [Message], error, clientnotverified, failed_to_catch_invalid_client).

request_client_group_return_only_clients_belonging_to_group_test(_Config) ->
    {skip, "Client groups are not yet implemented, request all client test used instead"}.

request_client_group_when_no_clients_with_group_return_empty_list_test(_Config) ->
    {skip, "Client groups are not yet implemented, request all client test used instead"}.