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
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test/1,
    get_clients_return_logged_in_client_test/1,
    register_client_with_taken_username_return_error_test/1,
    logout_client_test/1,
    register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests/1,
    client_is_logged_out_after_not_sending_heartbeat_test/1
]).

all() -> [
    login_non_existing_client_return_error_test,
    register_client_return_success_message_test,
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test,
    get_clients_return_logged_in_client_test,
    register_client_with_taken_username_return_error_test,
    logout_client_test,
    register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests,
    client_is_logged_out_after_not_sending_heartbeat_test
].

init_per_suite(Config) ->
    persistence_service:init(),

    NodeIPsAndKeys = [
        {"255.255.0.1", <<"publickey1">>},
        {"255.255.0.2", <<"publickey2">>},
        {"255.255.0.3", <<"publickey3">>}
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

login_client_return_succes_message_with_hash_and_dedicated_nodes_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    [] = DedicatedNodes.

get_clients_return_logged_in_client_test(Config) ->
    {Username, Password, _, _, _} = ?config(client, Config),
    {OtherUsername, OtherPassword, OtherPublicKey} = ?config(otherclient, Config),
    test_helpers_int:register_client(Username, Password),
    test_helpers_int:register_client(OtherUsername, OtherPassword),
    {OtherUsername, OtherPassword, OtherPublicKey, _, []} =
    test_helpers_int:login_client(OtherUsername, OtherPassword, OtherPublicKey),
    Request = {clientrequest, 1},

    {clientresponse, [
        {client, OtherUsername, OtherPublicKey, []}
    ]} = hrp_pb:decode_clientresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREQUEST', 'CLIENTRESPONSE')).

register_client_with_taken_username_return_error_test(Config) ->
    {Username, Password, _, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    OtherPassword = "Differentpassword1234",
    Request = {clientregisterrequest, Username, OtherPassword},

    {clientregisterresponse, 'TAKEN_USERNAME'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')).

logout_client_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, []} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    Request = {clientlogoutrequest, Username, SecretHash},
    {clientlogoutresponse, 'SUCCES'} = hrp_pb:decode_clientlogoutresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGOUTREQUEST', 'CLIENTLOGOUTRESPONSE')),

    {Username, undefined, PublicKey, Password, []} =
        persistence_service:select_client(Username).

register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests(Config) ->
    Username = "anotherusername",
    Password = "Password1234",
    PublicKey = <<"anotherpublickey">>,
    NodeIPsAndKeys = ?config(nodes, Config),
    NodeIds = [test_helpers_int:register_node_return_id(IP, Key) || {IP, Key} <- NodeIPsAndKeys],
    test_helpers_int:register_client(Username, Password),
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:login_client(Username, Password, PublicKey),

    true = test_helpers_int:valid_secret_hash(SecretHash),
    3 = length(lists:filter(fun(NodeId) -> lists:member(NodeId, DedicatedNodes) end, NodeIds)).

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
