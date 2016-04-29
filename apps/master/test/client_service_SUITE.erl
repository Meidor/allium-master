-module(client_service_SUITE).
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
    client_verify_existing_user/1, client_verify_non_existing_user/1
]).

all() -> [
    client_register_valid_client_return_ok_test,
    client_register_invalid_client_username_taken_return_error_test,
    client_register_unpredicted_error_return_error_test,
    client_verify_existing_user, client_verify_non_existing_user
].

init_per_suite(Config) ->
    meck:new(auth_service, [non_strict]),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    meck:unload(auth_service),
    Config.

client_register_valid_client_return_ok_test(_Config) ->
    ValidUsername = "ValidUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(auth_service, client_register, fun(_ValidUsername, _Password) -> ok end),

    ok = client_service:client_register(ValidUsername, Password),
    true = test_helpers:check_function_called(auth_service, client_register, [ValidUsername, Password]).

client_register_invalid_client_username_taken_return_error_test(_Config) ->
    InvalidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(auth_service, client_register, fun(_InvalidUsername, _Password) -> error(usernametaken) end),

    test_helpers:assert_fail(fun auth_service:client_register/2, [InvalidUsername, Password],
        error, usernametaken, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(auth_service, client_register, [InvalidUsername, Password]).

client_register_unpredicted_error_return_error_test(_Config) ->
    ValidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(auth_service, client_register, fun(_ValidUsername, _Password) -> error(somethingwentwrong) end),

    test_helpers:assert_fail(fun auth_service:client_register/2, [ValidUsername, Password],
        error, somethingwentwrong, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(auth_service, client_register, [ValidUsername, Password]).

client_verify_existing_user(_Config) ->
    Username = "Client1",
    SecretHash = "SECRETHASH123",
    meck:expect(auth_service, get_client_secret_hash, fun(Username) -> "SECRETHASH123" end),
    SecretHash = client_service:client_verify(Username, SecretHash).

client_verify_non_existing_user(_Config) ->
    Username = "Client2",
    SecretHash = "SECRETHASH123",
    meck:expect(auth_service, get_client_secret_hash, fun(Username) -> undefined end),
    test_helpers:assert_fail(fun client_service:client_verify/2, [Username, SecretHash],
        error, {badmatch, undefined}, user_with_username).