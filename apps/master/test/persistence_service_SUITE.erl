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
    insert_existing_client_perform_update_return_ok_test/1,
    select_existing_client_return_client_test/1,
    select_non_existing_client_return_undefined_test/1,
    select_all_existing_clients_while_two_clients_return_clients_test/1,
    select_all_existing_clients_while_no_existing_clients_return_empty_list_test/1,
    select_all_existing_clients_with_given_hash_two_matches_return_clients_test/1,
    select_all_existing_clients_with_given_hash_no_matches_return_empty_test/1,
    delete_existing_client_return_ok_test/1,
    delete_non_existing_client_return_ok_test/1,
    delete_all_clients_return_ok_test/1
]).

all() -> [
    insert_new_client_return_ok_test,
    insert_existing_client_perform_update_return_ok_test,
    select_existing_client_return_client_test,
    select_non_existing_client_return_undefined_test,
    select_all_existing_clients_while_two_clients_return_clients_test,
    select_all_existing_clients_while_no_existing_clients_return_empty_list_test,
    select_all_existing_clients_with_given_hash_two_matches_return_clients_test,
    select_all_existing_clients_with_given_hash_no_matches_return_empty_test,
    delete_existing_client_return_ok_test,
    delete_non_existing_client_return_ok_test,
    delete_all_clients_return_ok_test
].

init_per_suite(Config) ->
    persistence_service:init(),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    persistence_service:delete_all_clients(),
    Config.

insert_new_client_return_ok_test(_Config) ->
    ok = persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),

    {"Username", "SecretHash", "PublicKey", "Password"} = persistence_service:select_client("Username").

insert_existing_client_perform_update_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),

    persistence_service:insert_client("Username", "UpdatedHash", "PublicKey", "Password"),
    {"Username", "UpdatedHash", "PublicKey", "Password"} = persistence_service:select_client("Username").

select_existing_client_return_client_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),

    {"Username", "SecretHash", "PublicKey", "Password"} = persistence_service:select_client("Username").

select_non_existing_client_return_undefined_test(_Config) ->
    undefined = persistence_service:select_client("Username").

select_all_existing_clients_while_two_clients_return_clients_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),
    persistence_service:insert_client("Username2", "SecretHash", "PublicKey", "Password"),

    [{"Username2", "SecretHash", "PublicKey", "Password"},
        {"Username", "SecretHash", "PublicKey", "Password"}] = persistence_service:select_all_clients().

select_all_existing_clients_while_no_existing_clients_return_empty_list_test(_Config) ->
    [] = persistence_service:select_all_clients().

select_all_existing_clients_with_given_hash_two_matches_return_clients_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),
    persistence_service:insert_client("Username2", "AlternativeHash", "PublicKey", "Password"),
    persistence_service:insert_client("Username3", "SecretHash", "PublicKey", "Password"),

    [{"Username", "SecretHash", "PublicKey", "Password"},
        {"Username3", "SecretHash", "PublicKey", "Password"}] =  persistence_service:select_clients_by_hash("SecretHash").

select_all_existing_clients_with_given_hash_no_matches_return_empty_test(_Config) ->
    [] = persistence_service:select_clients_by_hash("SecretHash").

delete_existing_client_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),

    ok = persistence_service:delete_client("Username"),
    undefined = persistence_service:select_client("Username").

delete_non_existing_client_return_ok_test(_Config) ->
    ok = persistence_service:delete_client("Username").

delete_all_clients_return_ok_test(_Config) ->
    persistence_service:insert_client("Username", "SecretHash", "PublicKey", "Password"),
    persistence_service:insert_client("Username2", "SecretHash", "PublicKey", "Password"),

    ok = persistence_service:delete_all_clients().