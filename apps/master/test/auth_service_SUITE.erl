-module(auth_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_suite/1
]).

-export([
    client_register_valid_client_return_ok_test/1,
    client_register_invalid_client_username_taken_return_error_test/1,
    client_register_unpredicted_error_return_error_test/1,
    get_client_secret_hash_existing_user/1,
    get_client_secret_hash_not_existing_user/1
]).

all() -> [
    client_register_valid_client_return_ok_test,
    client_register_invalid_client_username_taken_return_error_test,
    client_register_unpredicted_error_return_error_test,
    get_client_secret_hash_existing_user,
    get_client_secret_hash_not_existing_user
].

init_per_suite(Config) ->
    meck:new(persistence_service, [non_strict]),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    meck:unload(persistence_service),
    Config.

get_client_secret_hash_existing_user(_Config) ->
    Username = "Client1",
    SecretHash = "SECRETHASH123",
    meck:expect(persistence_service, select_client, fun(Username) -> {"Client1", "SECRETHASH123", "PUBLICKEY123", "Qwerty123"} end),
    SecretHash = auth_service:get_client_secret_hash(Username).

get_client_secret_hash_not_existing_user(_Config) ->
    Username = "Client2",
    SecretHash = "SECRETHASH123",
    meck:expect(persistence_service, select_client, fun(Username) -> undefined end),
    test_helpers:assert_fail(fun auth_service:get_client_secret_hash/1, [Username],
        error, {badmatch, undefined}, no_user_with_username).


client_register_valid_client_return_ok_test(_Config) ->
    ValidUsername = "ValidUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_ValidUsername) -> undefined end),
    meck:expect(persistence_service, insert_client, fun(_ValidUsername, undefined, undefined, _Password) -> ok end),

    ok = auth_service:client_register(ValidUsername, Password),
    true = test_helpers:check_function_called(persistence_service, select_client, [ValidUsername]),
    true = test_helpers:check_function_called(persistence_service, insert_client,
        [ValidUsername, undefined, undefined, Password]).

client_register_invalid_client_username_taken_return_error_test(_Config) ->
    InvalidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_InvalidUsername) -> ["Username", "Hash", "Key", "Pass"] end),

    ok = test_helpers:assert_fail(fun auth_service:client_register/2, [InvalidUsername, Password],
        error, usernametaken, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(persistence_service, select_client, [InvalidUsername]).

client_register_unpredicted_error_return_error_test(_Config) ->
    ValidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_ValidUsername) -> error(oops) end),

    test_helpers:assert_fail(fun auth_service:client_register/2, [ValidUsername, Password],
        error, somethingwentwrong, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(persistence_service, select_client, [ValidUsername]).

