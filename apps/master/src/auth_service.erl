-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2
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

-spec client_verify(list(), list()) -> any().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    {User, ActualSecretHash, PublicKey, Password} = persistence_service:select_client(Username),
    SecretHash =:= ActualSecretHash.