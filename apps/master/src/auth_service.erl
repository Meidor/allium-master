-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) when is_list(Username), is_list(Password) ->
     persistence_service:insert_client(Username, undefined, undefined, Password).

-spec client_verify(list(), list()) -> any().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        {_, CorrectSecretHash, _, _} = persistence_service:select_client(Username),
        SecretHash = CorrectSecretHash,
        ok
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