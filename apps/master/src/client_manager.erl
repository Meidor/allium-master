-module(client_manager).

%% API
-export([
    return_all_clients_by_clientgroup/1
]).

%% @doc
%% Given a client group, this method returns all clients who belong to this particular group.
%% The client group must contain at least one client to be able to return his clients.
%% The passed is then passed on to the master_app_erl where it will be returned as a response on ClientRequest.
-spec return_all_clients_by_clientgroup(integer()) -> list().
return_all_clients_by_clientgroup(Clientgroup) when is_integer(Clientgroup), Clientgroup > 0 ->
    [{client, Username, PublicKey, ConnectedNodes} ||
        {Username, SecretHash, PublicKey, _, ConnectedNodes}
            <- persistence_service:select_all_clients(),
            SecretHash =/= undefined].