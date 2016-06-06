-module(client_manager).

%% API
-export([
    return_all_clients_by_clientgroup/1
]).

-spec return_all_clients_by_clientgroup(integer()) -> list().
%% @doc
%% Given a client group, this method returns all clients who belong to this particular group.
%% The clientgroup  is then passed on to the master_app where it will be returned as a response on ClientRequest.
%% params
%% Clientgroup: the clientgroup the returned clients belong to.
return_all_clients_by_clientgroup(Clientgroup) when is_integer(Clientgroup), Clientgroup > 0 ->
    [{client, Username, PublicKey, ConnectedNodes} ||
        {Username, SecretHash, PublicKey, _, ConnectedNodes}
            <- persistence_service:select_all_clients(),
            SecretHash =/= undefined].