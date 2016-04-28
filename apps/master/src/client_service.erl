-module(client_service).

%% API
-export([
    client_register/2
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) ->
    try auth_service:client_register(Username, Password) of
        _ ->
            ok
    catch
        error:usernametaken ->
            error(usernametaken);
        _:_ ->
            error(somethingwentwrong)
    end.
