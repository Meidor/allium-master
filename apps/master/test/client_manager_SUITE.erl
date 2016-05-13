-module(client_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
  all/0,
  init_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

-export([
  return_clients_by_hash_test/1
]).

all() -> [
  return_clients_by_hash_test
].

init_per_suite(Config) ->
  meck:new(persistence_service, [non_strict]),
  Config.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  meck:unload(persistence_service),
  Config.

return_clients_by_hash_test(_Config) ->
  meck:expect(persistence_service, select_all_clients, fun() -> [] end),

  [] = persistence_service:select_all_clients(),
  true = test_helpers:check_function_called(persistence_service, select_all_clients, []).