-module(redis_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_suite/1
]).

-export([
    get_value_existing_key_return_value_as_binary_test/1,
    get_value_non_existing_key_return_error_test/1,
    get_undefined_value_return_binary_test/1,
    set_value_existing_key_return_ok_test/1,
    set_value_non_existing_key_return_ok_test/1,
    set_value_to_atom_return_ok_test/1,
    get_list_with_empty_list_return_empty_list_test/1,
    get_list_with_list_all_non_existing_keys_return_empty_list_test/1,
    get_list_with_list_partly_non_existing_keys_return_only_existing_keyvalues_test/1,
    get_list_with_list_all_existing_keys_return_all_values_test/1,
    get_matching_key_with_no_existing_keys_return_empty_list/1,
    get_matching_key_with_only_matching_keys_return_all_keys_list/1,
    get_matching_key_with_all_but_one_matching_keys_return_all_but_one_keys_list/1,
    remove_existing_key_return_ok_and_one_removals_test/1,
    remove_non_existing_key_return_ok_and_zero_removal_test/1
]).

all() -> [
    get_value_existing_key_return_value_as_binary_test,
    get_value_non_existing_key_return_error_test,
    get_undefined_value_return_binary_test,
    set_value_existing_key_return_ok_test,
    set_value_non_existing_key_return_ok_test,
    set_value_to_atom_return_ok_test,
    get_list_with_empty_list_return_empty_list_test,
    get_list_with_list_all_non_existing_keys_return_empty_list_test,
    get_list_with_list_partly_non_existing_keys_return_only_existing_keyvalues_test,
    get_list_with_list_all_existing_keys_return_all_values_test,
    get_matching_key_with_no_existing_keys_return_empty_list,
    get_matching_key_with_only_matching_keys_return_all_keys_list,
    get_matching_key_with_all_but_one_matching_keys_return_all_but_one_keys_list,
    remove_existing_key_return_ok_and_one_removals_test,
    remove_non_existing_key_return_ok_and_zero_removal_test
].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    empty_database(),
    Config.

end_per_testcase(_, Config) ->
    Config.

get_value_existing_key_return_value_as_binary_test(_Config) ->
    redis:set("Key", "Value"),
    <<"Value">> = redis:get("Key").

get_value_non_existing_key_return_error_test(_Config) ->
    undefined = redis:get("Key").

get_undefined_value_return_binary_test(_Config) ->
    redis:set("Key", undefined),
    <<"undefined">> = redis:get("Key").

set_value_existing_key_return_ok_test(_Config) ->
    redis:set("Key", "Value"),
    {ok, <<"OK">>} = redis:set("Key", "OtherValue"),
    <<"OtherValue">> = redis:get("Key").

set_value_non_existing_key_return_ok_test(_Config) ->
    {ok, <<"OK">>} = redis:set("Key", "Value"),
    <<"Value">> = redis:get("Key").

set_value_to_atom_return_ok_test(_Config) ->
    {ok, <<"OK">>} = redis:set("Key", undefined),
    <<"undefined">> = redis:get("Key").

get_list_with_empty_list_return_empty_list_test(_Config) ->
    [] = redis:get_list([]).

get_list_with_list_all_non_existing_keys_return_empty_list_test(_Config) ->
    [undefined, undefined, undefined]  = redis:get_list(["onion_Key1", "onion_Key2", "onion_Key3"]).

get_list_with_list_partly_non_existing_keys_return_only_existing_keyvalues_test(_Config) ->
    redis:set("Key1", "Value1"),
    redis:set("Key3", "Value3"),
    [<<"Value1">>, undefined, <<"Value3">>]  = redis:get_list(["onion_Key1", "onion_Key2", "onion_Key3"]).

get_list_with_list_all_existing_keys_return_all_values_test(_Config) ->
    redis:set("Key1", "Value1"),
    redis:set("Key2", "Value2"),
    redis:set("Key3", "Value3"),
    [<<"Value1">>, <<"Value2">>, <<"Value3">>]  = redis:get_list(["onion_Key1", "onion_Key2", "onion_Key3"]).

get_matching_key_with_no_existing_keys_return_empty_list(_Config) ->
    [] = redis:get_matching_keys("non_existing").

get_matching_key_with_only_matching_keys_return_all_keys_list(_Config) ->
    redis:set("Key1", "Value1"),
    redis:set("Key2", "Value2"),
    redis:set("Key3", "Value3"),
    test_helpers:check_list_contains_values([<<"onion_Key1">>, <<"onion_Key2">>, <<"onion_Key3">>],
        [], redis:get_matching_keys("Key")).

get_matching_key_with_all_but_one_matching_keys_return_all_but_one_keys_list(_Config) ->
    redis:set("Key1", "Value1"),
    redis:set("Different1", "Different2"),
    redis:set("Key3", "Value3"),
    true = test_helpers:check_list_contains_values([<<"onion_Key1">>, <<"onion_Key3">>], [<<"onion_Key2">>],
        redis:get_matching_keys("Key")).

remove_existing_key_return_ok_and_one_removals_test(_Config) ->
    redis:set("Key1", "Value1"),
    {ok, <<"1">>} = redis:remove("Key1"),
    undefined = redis:get("Key1").

remove_non_existing_key_return_ok_and_zero_removal_test(_Config) ->
    {ok, <<"0">>} = redis:remove("Key1").

-spec empty_database() -> any().
empty_database() ->
    eredis:q(get_connection(), ["FLUSHALL"]).

-spec get_connection() -> pid().
get_connection() ->
    case whereis(redis) of
        undefined ->
            {ok, Connection} = eredis:start_link(),
            register(redis, Connection),
            Connection;
        Pid ->
            Pid
    end.