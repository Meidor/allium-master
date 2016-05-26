-module(client_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    return_logged_in_clients_by_clientgroup_when_no_clients_returns_empty_list_test/1,
    return_logged_in_clients_by_clientgroup_returns_list_of_logged_in_clients_test/1

]).

all() -> [
    return_logged_in_clients_by_clientgroup_when_no_clients_returns_empty_list_test,
    return_logged_in_clients_by_clientgroup_returns_list_of_logged_in_clients_test
].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    meck:new(persistence_service, [non_strict]),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(persistence_service),
    Config.

return_logged_in_clients_by_clientgroup_when_no_clients_returns_empty_list_test(_Config) ->
    meck:expect(persistence_service, select_all_clients, fun() -> [] end),
    [] = client_manager:return_all_clients_by_clientgroup(2),
    true = test_helpers:check_function_called(persistence_service, select_all_clients, []).

return_logged_in_clients_by_clientgroup_returns_list_of_logged_in_clients_test(_Config) ->
    meck:expect(persistence_service, select_all_clients, fun() ->
        [{"test2", "secrethash", <<"publickeytest">>, "pass2", []},
        {"test3", "othersecrethash", undefined, "pass3", []},
        {"test1", undefined, undefined, "pass1", []}] end),

    [{client, "test2", <<"publickeytest">>, []},
    {client, "test3", undefined, []}] = client_manager:return_all_clients_by_clientgroup(2),
    true = test_helpers:check_function_called(persistence_service, select_all_clients, []).


