-module(integration_client_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    login_non_existing_client_return_error_test/1,
    register_client_return_success_message_test/1,
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test/1,
    get_clients_return_registered_client_test/1,
    register_client_with_taken_username_return_error_test/1,
    logout_client_test/1,
    register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests/1,
    client_is_logged_out_after_not_sending_heartbeat_test/1
]).

all() -> [
    login_non_existing_client_return_error_test,
    register_client_return_success_message_test,
    login_client_return_succes_message_with_hash_and_dedicated_nodes_test,
    get_clients_return_registered_client_test,
    register_client_with_taken_username_return_error_test,
    logout_client_test,
    register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests,
    client_is_logged_out_after_not_sending_heartbeat_test
].

init_per_suite(Config) ->
    test_helpers_int:empty_database(),
    persistence_service:init(),
    persistence_service:delete_all_clients(),

    NodeIPsAndKeys = [{"255.255.0.1", <<"publickey1">>}, {"255.255.0.2", <<"publickey2">>},
        {"255.255.0.3", <<"publickey3">>}],
    ValidUsername = "username",
    ValidPassword = "Password1234",
    ValidClientPublicKey = <<"generatedpublickey">>,
    InvalidConnectedNodes = [],
    InvalidClientSecretHash = "secrethash6789",
    [{nodes, NodeIPsAndKeys},
        {client, {ValidUsername, ValidPassword, ValidClientPublicKey, InvalidClientSecretHash, InvalidConnectedNodes}}
    ] ++ Config.

end_per_suite(Config) ->
    test_helpers_int:empty_database(),
    persistence_service:delete_all_clients(),
    Config.

login_non_existing_client_return_error_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'INVALID_COMBINATION', "", []} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')).

register_client_return_success_message_test(Config) ->
    {Username, Password, _, _, _} = ?config(client, Config),
    Request = {clientregisterrequest, Username, Password},

    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')).

login_client_return_succes_message_with_hash_and_dedicated_nodes_test(Config) ->
    {Username, Password, PublicKey, _, _} = ?config(client, Config),
    Request = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    [] = DedicatedNodes,

    test_helpers_int:pass_to_next_test([{loggedinclient, {Username, Password, PublicKey, SecretHash, DedicatedNodes}}]).

get_clients_return_registered_client_test(Config) ->
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:retrieve_from_last_test(loggedinclient, Config),
    ClientGroup = 1, %%Serves no purpose right now
    Request = {clientrequest, ClientGroup},

    {clientresponse, [
        {client, Username, <<"generatedpublickey">>, []}
    ]} = hrp_pb:decode_clientresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREQUEST', 'CLIENTRESPONSE')),

    test_helpers_int:pass_to_next_test([{loggedinclient, {Username, Password, PublicKey, SecretHash, DedicatedNodes}}]).

register_client_with_taken_username_return_error_test(Config) ->
    {Username, Password, PublicKey, SecretHash, DedicatedNodes} =
        test_helpers_int:retrieve_from_last_test(loggedinclient, Config),
    OtherPassword = "Differentpassword1234",
    Request = {clientregisterrequest, Username, OtherPassword},

    {clientregisterresponse, 'TAKEN_USERNAME'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')),

    test_helpers_int:pass_to_next_test([{loggedinclient, {Username, Password, PublicKey, SecretHash, DedicatedNodes}}]).

logout_client_test(Config) ->
    {Username, Password, PublicKey, SecretHash, _DedicatedNodes} =
        test_helpers_int:retrieve_from_last_test(loggedinclient, Config),

    Request = {clientlogoutrequest, Username, SecretHash},
    {clientlogoutresponse, 'SUCCES'} = hrp_pb:decode_clientlogoutresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTLOGOUTREQUEST', 'CLIENTLOGOUTRESPONSE')),

    {Username, undefined, PublicKey, Password, []} =
        persistence_service:select_client(Username),

    test_helpers_int:pass_to_next_test([{existingclient, {Username, Password, PublicKey, undefined, []}}]).

register_new_client_and_log_in_while_nodes_exist_return_nodes_as_dedicated_nodes_tests(Config) ->
    Username = "anotherusername",
    Password = "Password1234",
    PublicKey = <<"anotherpublickey">>,
    Request = {clientregisterrequest, Username, Password},

    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')),

    NodeIPsAndKeys = ?config(nodes, Config),
    NodeIds = [test_helpers_int:register_node_return_id(IP, Key) || {IP, Key} <- NodeIPsAndKeys],
    LoginRequest = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(LoginRequest, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = test_helpers_int:valid_secret_hash(SecretHash),
    3 = length(lists:filter(fun(NodeId) -> lists:member(NodeId, DedicatedNodes) end, NodeIds)),

    test_helpers_int:pass_to_next_test([
        {loggedoutclient, test_helpers_int:retrieve_from_last_test(existingclient, Config)},
        {loggedinclient, {Username, Password, PublicKey, SecretHash, DedicatedNodes}}]).

client_is_logged_out_after_not_sending_heartbeat_test(Config) ->
      heartbeat_monitor_sup:start_link(),
    {Username, Password, PublicKey, undefined, []} =
        test_helpers_int:retrieve_from_last_test(loggedoutclient, Config),
    {UsernameTwo, PasswordTwo, PublicKeyTwo, SecretHashTwo, DedicatedNodesTwo} =
        test_helpers_int:retrieve_from_last_test(loggedinclient, Config),

    LoginRequest = {clientloginrequest, Username, Password, PublicKey},

    {clientloginresponse, 'SUCCES', _SecretHash, DedicatedNodesOne} = hrp_pb:decode_clientloginresponse(
        test_helpers_int:get_data_encrypted_response(LoginRequest, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),

    HeartbeatRequest = test_helpers_int:encode_message_to_binary({nodeheartbeat, UsernameTwo, SecretHashTwo}),

    timer:sleep(4000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'CLIENTHEARTBEAT'),
    timer:sleep(4000),
    test_helpers_int:send_heartbeat(HeartbeatRequest, 'CLIENTHEARTBEAT'),
    timer:sleep(4000),

    {Username, undefined, PublicKey, Password, DedicatedNodesOne} = persistence_service:select_client(Username),
    {UsernameTwo, SecretHashTwo, PublicKeyTwo, PasswordTwo, DedicatedNodesTwo} =
        persistence_service:select_client(UsernameTwo).
