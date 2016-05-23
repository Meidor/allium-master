-module(persistence_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_suite/1
]).

-export([
    insert_new_client_return_ok_test/1,
    insert_existing_client__return_error_test/1,
    update_existing_client_hash_return_ok_test/1,
    update_non_existing_client_hash_return_error_test/1,
    update_existing_client_return_ok_test/1,
    update_non_existing_client_return_error_test/1,
    select_existing_client_return_client_test/1,
    select_non_existing_client_return_undefined_test/1,
    select_all_existing_clients_while_two_clients_return_clients_test/1,
    select_all_existing_clients_while_no_existing_clients_return_empty_list_test/1,
    select_all_existing_clients_with_given_hash_two_matches_return_clients_test/1,
    select_all_existing_clients_with_given_hash_no_matches_return_empty_test/1,
    delete_existing_client_return_ok_test/1,
    delete_non_existing_client_return_ok_test/1,
    delete_all_clients_return_ok_test/1,
    select_admin_return_empty_test/1
]).

all() -> [
    insert_new_client_return_ok_test,
    insert_existing_client__return_error_test,
    update_existing_client_hash_return_ok_test,
    update_non_existing_client_hash_return_error_test,
    update_existing_client_return_ok_test,
    update_non_existing_client_return_error_test,
    select_existing_client_return_client_test,
    select_non_existing_client_return_undefined_test,
    select_all_existing_clients_while_two_clients_return_clients_test,
    select_all_existing_clients_while_no_existing_clients_return_empty_list_test,
    select_all_existing_clients_with_given_hash_two_matches_return_clients_test,
    select_all_existing_clients_with_given_hash_no_matches_return_empty_test,
    delete_existing_client_return_ok_test,
    delete_non_existing_client_return_ok_test,
    delete_all_clients_return_ok_test,
    select_admin_return_empty_test
].

init_per_suite(Config) ->
    test_helpers_int:init_sharded_eredis(),
    persistence_service:init(),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    persistence_service:delete_all_clients(),
    Config.

insert_new_client_return_ok_test(_Config) ->
    ok = persistence_service:insert_client("Username", "Password"),

    {"Username", undefined, undefined, "Password", []} = persistence_service:select_client("Username").

insert_existing_client__return_error_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),

    test_helpers:assert_fail(fun persistence_service:insert_client/2, ["Username", "Pass"],
        error, usernametaken, failed_to_catch_taken_username),
    {"Username", undefined, undefined, "Password", []} = persistence_service:select_client("Username").

update_existing_client_hash_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),
    persistence_service:insert_client("Username2", "Password"),

    ok = persistence_service:update_client_hash("Username", "NewHash"),
    [{"Username2", undefined, undefined, "Password", []}, {"Username", "NewHash", undefined,
        "Password", []}] = persistence_service:select_all_clients().

update_non_existing_client_hash_return_error_test(_Config) ->
    test_helpers:assert_fail(fun persistence_service:update_client_hash/2, ["Username", "SecretHash"],
        error, couldnotbeupdated, client_does_not_exist).

update_existing_client_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),
    persistence_service:insert_client("Username2", "Password"),

    ok = persistence_service:update_client("Username", "NewHash", <<"PublicKey">>, ["1", "11", "111"]),
    [{"Username2", undefined, undefined, "Password", []}, {"Username", "NewHash", <<"PublicKey">>,
        "Password", ["1", "11", "111"]}] = persistence_service:select_all_clients().

update_non_existing_client_return_error_test(_Config) ->
    test_helpers:assert_fail(fun persistence_service:update_client/4, ["Username", "SecretHash", <<"PublicKey">>, ["1", "11", "111"]],
        error, couldnotbeupdated, client_does_not_exist).

select_existing_client_return_client_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),

    {"Username", undefined, undefined, "Password", []} = persistence_service:select_client("Username").

select_non_existing_client_return_undefined_test(_Config) ->
    undefined = persistence_service:select_client("Username").

select_all_existing_clients_while_two_clients_return_clients_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),
    persistence_service:insert_client("Username2", "Password"),

    [{"Username2", undefined, undefined, "Password", []},
        {"Username", undefined, undefined,"Password", []}] = persistence_service:select_all_clients().

select_all_existing_clients_while_no_existing_clients_return_empty_list_test(_Config) ->
    [] = persistence_service:select_all_clients().

select_all_existing_clients_with_given_hash_two_matches_return_clients_test(_Config) ->
    persistence_service:insert_client("Username","Password"),
    persistence_service:insert_client("Username2", "Password"),
    persistence_service:insert_client("Username3", "Password"),
    persistence_service:update_client_hash("Username", "SecretHash"),
    persistence_service:update_client_hash("Username3", "SecretHash"),

    [{"Username", "SecretHash", undefined, "Password", []},
        {"Username3", "SecretHash", undefined, "Password", []}] =  persistence_service:select_clients_by_hash("SecretHash").

select_all_existing_clients_with_given_hash_no_matches_return_empty_test(_Config) ->
    [] = persistence_service:select_clients_by_hash("SecretHash").

delete_existing_client_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),

    ok = persistence_service:delete_client("Username"),
    undefined = persistence_service:select_client("Username").

delete_non_existing_client_return_ok_test(_Config) ->
    ok = persistence_service:delete_client("Username").

delete_all_clients_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "Password"),
    persistence_service:insert_client("Username2",  "Password"),

    ok = persistence_service:delete_all_clients(),
    [] = persistence_service:select_all_clients().

select_admin_return_empty_test(_Config) ->
    Username = "Username",
    undefined = persistence_service:select_admin(Username).