-module(client_manager).

%% API
-export([
  return_all_clients_by_hash/1
]).

-spec return_all_clients_by_hash(integer()) -> list().
return_all_clients_by_hash(Hash) when Hash > 0 ->
  [{Username, PublicKey, ConnectedNodes} ||
    {Username, _, PublicKey, _, ConnectedNodes} <- persistence_service:select_all_clients()].

