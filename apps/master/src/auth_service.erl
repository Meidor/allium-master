-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1,
    client_login/3
]).

-define(AMOUNTOFDEDICATEDNODES, 5).

-spec client_register(list(), list()) -> any().
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    persistence_service:insert_client(Username, Password).

-spec client_verify(list(), list()) -> any().
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        {_, SecretHash, _, _, _} = persistence_service:select_client(Username)
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
            error(clientcredentialsnotvalid)
    end.

-spec client_logout(list()) -> any().
client_logout(Username) when is_list(Username) ->
    try
        persistence_service:update_client_hash(Username, undefined)
    catch
        _:_ ->
            error(couldnotbeloggedout)
    end.

-spec client_login(list(), list(), binary())-> any().
client_login(Username, Password, PublicKey) when is_list(Username), is_list(Password), is_binary(PublicKey) ->
    client_checkpassword(Username, Password),
    AmountOfDedicatedNodes = ?AMOUNTOFDEDICATEDNODES,
    SecretHash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
    DedicatedNodes = node_graph_manager:get_random_dedicated_nodes(AmountOfDedicatedNodes),
    persistence_service:update_client(Username, SecretHash, PublicKey, DedicatedNodes),
    {SecretHash, DedicatedNodes}.