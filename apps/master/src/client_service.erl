-module(client_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1,
    client_login/3,
    client_logout/2
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    auth_service:client_register(Username, Password).

-spec client_verify(list(), list()) -> list().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    auth_service:client_verify(Username, SecretHash).

-spec client_logout(list()) -> any().
client_logout(Username) when is_list(Username) ->
    auth_service:client_logout(Username),
    heartbeat_monitor:remove_client(Username).

-spec client_login(list(), list(), binary()) -> any().
client_login(Username, Password, PublicKey)
    when
        is_list(Username), is_list(Password), is_binary(PublicKey)
    ->
    Response = auth_service:client_login(Username, Password, PublicKey),
    heartbeat_monitor:add_client(Username),
    Response.

-spec client_logout(list(), list()) -> any().
client_logout(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    client_verify(Username, SecretHash),
    client_logout(Username).

