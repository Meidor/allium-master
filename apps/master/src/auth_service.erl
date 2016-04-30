-module(auth_service).

%% API
-export([
    client_register/2
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) ->
    lager:info("Client registration started..."),
    try persistence_service:select_client(Username) of
        undefined ->
            lager:info("Client registered..."),
            persistence_service:insert_client(Username, undefined, undefined, Password);
        _ ->
            error(usernametaken)
    catch
        _:_ ->
            lager:info("Client registration did not work..."),
            error(somethingwentwrong)
    end.

