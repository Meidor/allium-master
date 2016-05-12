-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1,
    client_login/3,
    client_checkpassword/2
]).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    persistence_service:insert_client(Username, Password).

-spec client_verify(list(), list()) -> any().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        {_, SecretHash, _, _} = persistence_service:select_client(Username)
    catch
        _:_ ->
            error(clientnotverified)
    end.

-spec client_checkpassword(list(), list()) -> any().
client_checkpassword(Username, Password) when is_list(Username), is_list(Password) ->
    try
        {_, _, _, Password, _} = persistence_service:select_client(Username)
    catch
        _:_ ->
            error(invalidclient)
    end.

-spec client_logout(list()) -> any().
client_logout(Username) when is_list(Username) ->
    try
        persistence_service:update_client_hash(Username, undefined)
    catch
        _:_ ->
            error(couldnotbeloggedout)
    end.

-spec client_login(list(), list(), list())-> any().
client_login(Username, Password, PublicKey) when is_list(Username), is_list(Password), is_list(PublicKey) ->
    try
        NumberOfDedicatedNodes = 5,
        client_checkpassword(Username, Password),
        %%todo: placeholder secrethash vervangen
        SecretHash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
        persistence_service:update_client_hash(Username, SecretHash),
        persistence_service:update_client_publickey(Username, PublicKey),
        %%todo: placeholder bij redis (alle) nodes ophalen
        DedicatedNodes = node_graph_manager:get_random_dedicatednodes(NumberOfDedicatedNodes),
%%        {"node1","node2","node3","node4","node5"},
        persistence_service:update_client_dedicatednodes(Username, DedicatedNodes)
    catch
        _:_ ->
            error(couldnotlogin)
    end.