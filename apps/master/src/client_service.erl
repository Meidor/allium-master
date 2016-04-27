-module(client_service).

%% API
-export([
    client_register/2
]).

client_register(Username, Password) ->
    try auth_service:client_register(Username, Password) of
        _ ->
            ok
        catch _:_ ->
            {error, "Username is already taken"}
    end.
