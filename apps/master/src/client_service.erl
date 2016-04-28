-module(client_service).

%% API
-export([
    client_register/2,
    client_verify/2
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

-spec client_verify(list(), list()) -> list().
client_verify(Username, Password) when is_list(Username), is_list(Password) ->
    ok.
