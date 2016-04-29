-module(auth_service).

%% API
-export([
    client_register/2,
    get_client_secret_hash/1
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) ->
    try persistence_service:select_client(Username) of
        undefined ->
            persistence_service:insert_client(Username, undefined, undefined, Password);
        _ ->
            error(usernametaken)
    catch
        _:_ ->
            error(somethingwentwrong)
    end.

-spec get_client_secret_hash(list()) -> list().
get_client_secret_hash(Username) when is_list(Username) ->
    {User, SecretHash, PublicKey, Password} = persistence_service:select_client(Username),
    SecretHash.
