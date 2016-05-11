-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1
    , client_return_all_clients_by_hash/1]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    persistence_service:insert_client(Username, Password).

-spec client_verify(list(), list()) -> any().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        {_, SecretHash, _, _} = persistence_service:select_client(Username)
    catch
        _:_ ->
            error(clientnotverified)
    end.

-spec client_logout(list()) -> any().
client_logout(Username) when is_list(Username) ->
    try
        persistence_service:update_client_hash(Username, undefined)
    catch
        _:_ ->
            error(couldnotbeloggedout)
    end.

%% Todo: Change select_all_clients to it's own function. for iteration 3.
-spec client_return_all_clients_by_hash(int32) -> [].
client_return_all_clients_by_hash(Hash) when Hash > 0 ->
    try
        {Username, _, PublicKey, _, ConnectedNodes} = persistence_service:select_all_clients()
    catch
        _:_  ->
            error(noClientsForHash)
    end.