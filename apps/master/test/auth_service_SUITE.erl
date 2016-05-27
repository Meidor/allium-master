-module(auth_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_suite/1
]).

-export([
    client_verify_existing_user_wrong_hash/1,
    client_register_valid_client_return_ok_test/1,
    client_register_invalid_client_username_taken_return_error_test/1,
    client_verify_existing_user/1,
    client_verify_not_existing_user/1,
    client_logout_return_ok_test/1,
    non_existing_client_logout_return_ok_test/1,
    client_login_return_secret_hash_and_dedicated_nodes_test/1,
    client_login_with_wrong_password_return_client_not_verified_test/1,
    admin_login_return_true_test/1,
    admin_login_return_false_test/1,
    admin_login_with_wrong_password_return_admin_not_verified_test/1,
    verify_super_admin_with_super_admin_return_ok_test/1,
    verify_super_admin_with_regular_admin_return_error_test/1,
    verify_super_admin_with_non_existing_admin_return_error_test/1
]).

all() -> [
    client_verify_existing_user_wrong_hash,
    client_register_valid_client_return_ok_test,
    client_register_invalid_client_username_taken_return_error_test,
    client_verify_existing_user,
    client_verify_not_existing_user,
    client_logout_return_ok_test,
    non_existing_client_logout_return_ok_test,
    client_login_return_secret_hash_and_dedicated_nodes_test,
    client_login_with_wrong_password_return_client_not_verified_test,
    admin_login_return_true_test,
    admin_login_return_false_test,
    admin_login_with_wrong_password_return_admin_not_verified_test,
    verify_super_admin_with_super_admin_return_ok_test,
    verify_super_admin_with_regular_admin_return_error_test,
    verify_super_admin_with_non_existing_admin_return_error_test
].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(persistence_service, [non_strict]),
    meck:new(node_graph_manager, [non_strict]),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(persistence_service),
    meck:unload(node_graph_manager),
    Config.

client_verify_existing_user(_Config) ->
    Username = "Client1",
    SecretHash = "SECRETHASH123",
    meck:expect(persistence_service, select_client, fun(_Username) ->
        {Username, SecretHash, <<"PUBLICKEY123">>, "Qwerty123", []} end),

    auth_service:client_verify(Username, SecretHash),
    true = test_helpers:check_function_called(persistence_service, select_client, [Username]).

client_verify_existing_user_wrong_hash(_Config) ->
    Username = "Client1",
    SecretHash = "SECRETHASH123",
    meck:expect(persistence_service, select_client, fun(_Username) ->
        {Username, "OTHERSECRETHASH", <<"PUBLICKEY123">>, "Qwerty123", []} end),

    test_helpers:assert_fail(fun auth_service:client_verify/2, [Username, SecretHash],
        error, clientnotverified, no_user_with_username),
    true = test_helpers:check_function_called(persistence_service, select_client, [Username]).

client_verify_not_existing_user(_Config) ->
    Username = "Client2",
    SecretHash = "SECRETHASH123",
    meck:expect(persistence_service, select_client, fun(_Username) -> undefined end),

    test_helpers:assert_fail(fun auth_service:client_verify/2, [Username, SecretHash],
        error, clientnotverified, no_user_with_username),
    true = test_helpers:check_function_called(persistence_service, select_client, [Username]).

client_register_valid_client_return_ok_test(_Config) ->
    ValidUsername = "ValidUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_ValidUsername) -> undefined end),
    meck:expect(persistence_service, insert_client, fun(_ValidUsername, _Password) -> ok end),

    ok = auth_service:client_register(ValidUsername, Password),
    true = test_helpers:check_function_called(persistence_service, insert_client,
        [ValidUsername, Password]).

client_register_invalid_client_username_taken_return_error_test(_Config) ->
    InvalidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_InvalidUsername) ->
        {InvalidUsername, undefined, undefined, Password, []} end),
    meck:expect(persistence_service, insert_client, fun(_ValidUsername, _Password) -> error(usernametaken) end),

    test_helpers:assert_fail(fun auth_service:client_register/2, [InvalidUsername, Password],
        error, usernametaken, failed_to_catch_invalid_username).

client_logout_return_ok_test(_Config) ->
    ValidUsername = "Username",
    meck:expect(persistence_service, update_client_hash, fun(_ValidUsername, undefined) -> ok end),

    ok = auth_service:client_logout(ValidUsername),
    true = test_helpers:check_function_called(persistence_service, update_client_hash, [ValidUsername, undefined]).

non_existing_client_logout_return_ok_test(_Config) ->
    InvalidUsername = "TakenUsername",
    meck:expect(persistence_service, update_client_hash, fun(_, undefined) -> error(couldnotbeupdated) end),

    test_helpers:assert_fail(fun auth_service:client_logout/1, [InvalidUsername],
        error, couldnotbeloggedout, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(persistence_service, update_client_hash, [InvalidUsername, undefined]).

client_login_return_secret_hash_and_dedicated_nodes_test(_Config) ->
    ValidUsername = "Username",
    ValidPassword = "Password123",
    ValidPublicKey = <<"PublicKey@123">>,
    DedicatedNodes = ["node1","node2","node3","node4","node5"],
    AmountOfDedicatedNodes = 5,
    meck:expect(persistence_service, update_client, fun(_Username, _SecretHash, _publicKey, _DedicatedNodes) -> ok end),
    meck:expect(persistence_service, select_client, fun(_Username) ->
        {ValidUsername, undefined, ValidPublicKey, ValidPassword, []} end),
    meck:expect(node_graph_manager, get_random_dedicated_nodes, fun(_AmountOfNodes) -> DedicatedNodes end),
    {SecretHash, Nodes} = auth_service:client_login(ValidUsername, ValidPassword, ValidPublicKey),
    true = test_helpers:check_function_called(persistence_service, update_client,
        [ValidUsername, SecretHash, ValidPublicKey, Nodes]),
    true = test_helpers:check_function_called(node_graph_manager, get_random_dedicated_nodes, [AmountOfDedicatedNodes]).

admin_login_return_true_test(_Config) ->
    ValidUsername = "Username",
    ValidPassword = "Password123",
    SuperAdmin = true,
    meck:expect(persistence_service, select_admin, fun(_Username) ->
        {ValidUsername, ValidPassword, SuperAdmin} end),
    true = auth_service:admin_login(ValidUsername, ValidPassword),
    true = test_helpers:check_function_called(persistence_service, select_admin,
        [ValidUsername]).

admin_login_return_false_test(_Config) ->
    ValidUsername = "Username",
    ValidPassword = "Password123",
    SuperAdmin = false,
    meck:expect(persistence_service, select_admin, fun(_Username) ->
        {ValidUsername, ValidPassword, SuperAdmin} end),
    false = auth_service:admin_login(ValidUsername, ValidPassword),
    true = test_helpers:check_function_called(persistence_service, select_admin,
        [ValidUsername]).

client_login_with_wrong_password_return_client_not_verified_test(_Config) ->
    ValidUsername = "Username1234567890",
    InvalidPassword = "Password123",
    ValidPublicKey = <<"PublicKey@123">>,
    meck:expect(persistence_service, select_client, fun(_Username) -> undefined end),
    test_helpers:assert_fail(fun auth_service:client_login/3, [ValidUsername, InvalidPassword, ValidPublicKey],
        error, clientcredentialsnotvalid, failed_to_catch_invalid_password).

admin_login_with_wrong_password_return_admin_not_verified_test(_Config) ->
    ValidUsername = "Username",
    InvalidPassword = "Password123",
    meck:expect(persistence_service, select_admin, fun(_Username) -> undefined end),
    test_helpers:assert_fail(fun auth_service:admin_login/2, [ValidUsername, InvalidPassword],
        error, admincredentialsnotvalid, failed_to_catch_invalid_password).

verify_super_admin_with_super_admin_return_ok_test(_Config) ->
    Username = "SuperAdmin",
    meck:expect(persistence_service, select_admin, fun(_) -> {Username, "Password", true} end),
    auth_service:verify_super_admin(Username),
    true = test_helpers:check_function_called(persistence_service, select_admin,
        [Username]).

verify_super_admin_with_regular_admin_return_error_test(_Config) ->
    Username = "RegularAdmin",
    meck:expect(persistence_service, select_admin, fun(_) -> {Username, "Password", false} end),
    test_helpers:assert_fail(fun auth_service:verify_super_admin/1, [Username],
        error, {badmatch,{"RegularAdmin","Password", false}}, failed_to_catch_regular_admin),
    true = test_helpers:check_function_called(persistence_service, select_admin,
        [Username]).

verify_super_admin_with_non_existing_admin_return_error_test(_Config) ->
    Username = "NonExistingAdmin",
    meck:expect(persistence_service, select_admin, fun(_) -> undefined end),
    test_helpers:assert_fail(fun auth_service:verify_super_admin/1, [Username],
        error, {badmatch, undefined}, failed_to_catch_regular_admin),
    true = test_helpers:check_function_called(persistence_service, select_admin,
        [Username]).