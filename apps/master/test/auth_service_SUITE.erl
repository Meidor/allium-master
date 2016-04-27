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
    client_register_invalid_client_username_taken_return_error_test/1
]).

all() -> [
    client_register_valid_client_return_ok_test,
    client_register_invalid_client_username_taken_return_error_test
].

init_per_suite(Config) ->
    meck:new(persistence_service, [non_strict]),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    meck:unload(persistence_service),
    Config.

client_register_valid_client_return_ok_test(_Config) ->
    ValidUsername = "ValidUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_ValidUsername) -> undefined end),
    meck:expect(persistence_service, insert_client, fun(_ValidUsername, _undefined, _undefined, _Password) -> ok end),

    ok = auth_service:client_register(ValidUsername, Password),
    true = test_helpers:check_function_called(persistence_service, select_client, [ValidUsername]),
    true = test_helpers:check_function_called(persistence_service, insert_client,
        [ValidUsername, undefined, undefined, Password]).

client_register_invalid_client_username_taken_return_error_test(_Config) ->
    InvalidUsername = "TakenUsername",
    Password = "jiddSDIH#FJSOE=-0==fdIHDSihe(HIFj*Dufnkdknfzsi(U(W*jf",
    meck:expect(persistence_service, select_client, fun(_ValidUsername) -> ["Username", "Hash", "Key", "Pass"] end),

    ok = test_helpers:assert_fail(fun auth_service:client_register/2, [InvalidUsername, Password],
        error, {badmatch, ["Username", "Hash", "Key", "Pass"]}, failed_to_catch_invalid_username),
    true = test_helpers:check_function_called(persistence_service, select_client, [InvalidUsername]).
