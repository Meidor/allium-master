-module(client_manager).

%% API
-export([
    return_all_clients_by_clientgroup/1
]).

-spec return_all_clients_by_clientgroup(integer()) -> list().
return_all_clients_by_clientgroup(Clientgroup) when is_integer(Clientgroup), Clientgroup > 0 ->
    [{Username, PublicKey, ConnectedNodes} ||
        {Username, _, PublicKey, _, ConnectedNodes} <- persistence_service:select_all_clients()].


