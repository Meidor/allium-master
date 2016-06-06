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
%% @doc
%% Allows for the registration of clients. The passed information is checked if valid username,
%% then passed on to the auth_service.
%% params
%% Username: Username of the client.
%% Password: Password of the client.
%% errors
%% username_invalid: when the given username does not match constraints.
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    ok = verify_username(Username),
    auth_service:client_register(Username, Password).

-spec verify_username(list()) -> atom().
verify_username(Username)
    when
        is_list(Username), length(Username) > 2, length(Username) =< 40
    ->
    {ok, MP} = re:compile("^[a-zA-Z0-9_-]*$"),
    case re:run(Username, MP) of
        nomatch -> error(username_invalid);
        _Else -> ok
    end.

-spec client_verify(list(), list()) -> list().
%% @doc
%% Checks whether the secret hash matches the known secret hash for the Username.
%% params
%% Username: Username of the client.
%% SecretHash: Secret hash of the client.
%% errors
%% clientnotverified: when there is no client that matches the given username.
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    auth_service:client_verify(Username, SecretHash).

-spec client_logout(list()) -> any().
%% @doc
%% Allows for the log out of clients. First checks the passed information, logs out client and
%% then removes the accompanying heartbeat.
%% params
%% Username: Username of the client.
%% errors
%% couldnotbeloggedout: When clients secrethash could not be set to undefined.
client_logout(Username) when is_list(Username) ->
    auth_service:client_logout(Username),
    heartbeat_monitor:remove_client(Username),
    ok.

-spec client_login(list(), list(), binary()) -> any().
%% @doc
%% Allows for the login of clients. The passed information is checked, which will return a response.
%% Also starts the heartbeat monitor for the client.
%% params
%% Username: Username of the client.
%% Password: Password of the client.
%% PublicKey: Public key of the client.
%% errors
%% clientcredentialsnotvalid: When wrong password is entered.
%% couldnotbeupdated: When Mnesia can not update the client.
client_login(Username, Password, PublicKey)
    when
        is_list(Username), is_list(Password), is_binary(PublicKey)
    ->
    Response = auth_service:client_login(Username, Password, PublicKey),
    heartbeat_monitor:add_client(Username),
    Response.

-spec client_logout(list(), list()) -> any().
%% @doc
%% Allows for the log out of clients. First checks the passed information, then removes the accompanying heartbeat.
%% params
%% Username: Username of the client.
%% SecretHash: Secret hash of client.
%% errors
%% clientnotverified: When there is no client that matches the given username.
%% couldnotbeloggedout: When clients secrethash could not be set to undefined.
client_logout(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    client_verify(Username, SecretHash),
    client_logout(Username).

