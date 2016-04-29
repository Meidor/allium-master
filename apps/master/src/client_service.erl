-module(client_service).

%% API
-export([
    client_register/2
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) ->
    auth_service:client_register(Username, Password).