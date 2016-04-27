-module(auth_service).

%% API
-export([
    client_register/2
]).

client_register(Username, Password) ->
    undefined = persistence_service:select_client(Username),
    persistence_service:insert_client(Username, undefined, undefined, Password).

