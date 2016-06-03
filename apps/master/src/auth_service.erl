-module(auth_service).

%% API
-export([
    client_register/2,
    client_verify/2,
    client_logout/1,
    client_login/3,
    admin_login/2,
    verify_super_admin/1]).

-define(AMOUNTOFDEDICATEDNODES, 5).

-spec client_register(list(), list()) -> any().
%% @doc
%% Allows for the registration of clients. The passed information is checked, then passed on to the auth_service.
%% params
%% Username: username of the client.
%% Password: password of the client. 
%% errors
%% usernametaken: when username is taken.
%% couldnotbeinserted: when there is an error inside mnesia.
client_register(Username, Password) when is_list(Username), is_list(Password) ->
    persistence_service:insert_client(Username, Password).

-spec client_verify(list(), list()) -> any().
%% @doc
%% Checks whether secret hash matches the known secret hash for the username.
%% params
%% Username: username of the client.
%% SecretHash: secretHash of the client. 
%% errors
%% clientnotverified: when client is not verified.
client_verify(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        {_, SecretHash, _, _, _} = persistence_service:select_client(Username)
    catch
        _:_ ->
            error(clientnotverified)
    end.

-spec client_check_password_and_return_dedicated_nodes(list(), list()) -> any().
client_check_password_and_return_dedicated_nodes(Username, Password) when is_list(Username), is_list(Password) ->
    try
        {_, _, _, Password, DedicatedNodes} = persistence_service:select_client(Username),
        DedicatedNodes
    catch
        _:_ ->
            error(clientcredentialsnotvalid)
    end.

-spec client_logout(list()) -> any().
%% @doc
%% Sets the secret hash of the user to undefined.
%% If client has been updated he will be logged out.
%% params
%% Username: username of the client.
%% errors
%% couldnotbeupdated: when client could not be updated.
%% couldnotbeloggedout: when client could net be logged in (when client could not be updated.)
client_logout(Username) when is_list(Username) ->
    try
        persistence_service:update_client_hash(Username, undefined)
    catch
        _:_ ->
            error(couldnotbeloggedout)
    end.

-spec client_login(list(), list(), binary())-> any().
%% @doc
%% Allows for the login of clients. The passed information is checked in a couple of ways.
%% A couple of random nodes will be assigned to the client, after that, a secret hash will be generated.
%% These variables will be added to the clients user credentials and when this is done the secret hash and
%% dedicated nodes will be returned.
%% params
%% Username: username of the client.
%% Password: password of the client.
%% PublicKey: publickey of the cient.
%% errors
%% couldnotbeupdated: when client could not be updated.
%% clientcredentialsnotvalid: when the info of the client could not be matched to a registered client.
client_login(Username, Password, PublicKey)
    when
        is_list(Username), is_list(Password), is_binary(PublicKey)
    ->
    CurrentNodes = client_check_password_and_return_dedicated_nodes(Username, Password),
    DedicatedNodes = assign_dedicated_nodes(CurrentNodes),
    SecretHash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
    persistence_service:update_client(Username, SecretHash, PublicKey, DedicatedNodes),
    {SecretHash, DedicatedNodes}.

-spec admin_login(list(), list())-> true | false.
%% @doc
%% Checks whether the password matches the known password for the username.
%% If the admin exists in the system his super admin status will be returned.
%% params
%% Username: username of the admin.
%% Password: password of the admin.
%% errors
%% admincredentialsnotvalid: when the info of the admin could not be matched to a registered.
admin_login(Username, Password)
    when
    is_list(Username), is_list(Password) ->
    admin_check_password_and_return_super_admin(Username, Password).

-spec admin_check_password_and_return_super_admin(list(), list()) -> true | false.
admin_check_password_and_return_super_admin(Username, Password)
    when
        is_list(Username),
        is_list(Password)
    ->
    try
        {Username, Password, SuperAdmin} = persistence_service:select_admin(Username),
        SuperAdmin
    catch
        _:_ ->
            error(admincredentialsnotvalid)
    end.

-spec verify_super_admin(list()) -> any().
%% @doc
%% Checks whether the admin is a super admin.
%% params
%% Username: username of the admin.
verify_super_admin(Username)
    when
    is_list(Username) ->
    {Username, _, true} = persistence_service:select_admin(Username).

-spec assign_dedicated_nodes(list()) -> list().
assign_dedicated_nodes(CurrentNodes) ->
    AmountOfDedicatedNodes = ?AMOUNTOFDEDICATEDNODES,
    try
        [] = CurrentNodes,
        node_graph_manager:get_random_dedicated_nodes(AmountOfDedicatedNodes)
    catch
        _:_ ->
            CurrentNodes
    end.

