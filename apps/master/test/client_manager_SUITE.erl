-module(client_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    return_clients_by_clientgroup_empty_string_test/1,
    return_clients_by_clientgroup_with_valid_response_test/1

]).

all() -> [
    return_clients_by_clientgroup_empty_string_test,
    return_clients_by_clientgroup_with_valid_response_test
].

init_per_suite(Config) ->
    meck:new(persistence_service, [non_strict]),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    meck:unload(persistence_service),
    Config.

return_clients_by_clientgroup_empty_string_test(_Config) ->
    meck:expect(persistence_service, select_all_clients, fun() -> [] end),
    [] = client_manager:return_all_clients_by_clientgroup(2),
    true = test_helpers:check_function_called(persistence_service, select_all_clients, []).

return_clients_by_clientgroup_with_valid_response_test(_Config) ->
    meck:expect(persistence_service, select_all_clients, fun() ->
        [{"test2",  undefined, <<"publickeytest">>, "pass2", []},
        {"test3", undefined, undefined, "pass3", []},
        {"test1", undefined, undefined, "pass1", []}] end),

    [{"test2", <<"publickeytest">>, []},
    {"test3", undefined, []},
    {"test1", undefined, []}] = client_manager:return_all_clients_by_clientgroup(2),
    true = test_helpers:check_function_called(persistence_service, select_all_clients, []).


