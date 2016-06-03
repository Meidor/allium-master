-module(persistence_service).

%% API
-export([
    init/0,
    insert_client/2,
    update_client_hash/2,
    select_client/1,
    select_all_clients/0,
    delete_client/1,
    delete_all_clients/0,
    select_clients_by_hash/1,
    update_client/4,
    select_admin/1,
    insert_admin/1,
    update_admin/4,
    delete_admin/1,
    select_all_admins/0,
    select_all_admins_including_passwords/0,
    delete_all_admins/0
]).

-include_lib("stdlib/include/qlc.hrl").

-record(client, {username, secrethash, publickey, password, dedicatednodes = []}).
-record(admin, {username, password, superadmin}).

-spec init() -> any().
%% @doc
%% Mnesia starts, creates schemes for admins and clients, inserts a default super admin if no super admin is present.
init() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(admin,
        [ {disc_copies, [node()]},
            {attributes,
                record_info(fields, admin)}]),
    mnesia:create_table(client,
        [ {disc_copies, [node()] },
            {attributes,
                record_info(fields, client)}]),

    timer:sleep(1000),

    case length(select_all_super_admins()) of
        0 ->
            insert_admin("admin"),
            update_admin("admin", "password", true, false);
        _ ->
            ok
    end.

-spec insert_client(list(), list()) -> atom().
%% @doc
%% Given a username and password of a client, the method checks if username already exist.
%% If this is not the case, the client will be added to the database.
%% error
%% usernametaken: thrown when the client username is already taken.
%% couldnotbeinserted: thrown when the client could not be inserted for another reason than usernametaken.
%% param
%% Username: username of the client.
%% Password: password of the client.
insert_client(Username, Password) when is_list(Username), is_list(Password) ->
    try
        undefined = select_client(Username)
    catch
        _:{badmatch,_} ->
            error(usernametaken)
    end,

    case mnesia:transaction(fun() ->
        mnesia:write(
            #client{username = Username,
            password = Password})
        end) of
            {atomic, ok} ->
                ok;
            _ ->
                error(couldnotbeinserted)
        end.

-spec update_client(list(), list(), binary(), list()) -> atom().
%% @doc
%% Given the user credentials of a client, the method checks if this client is already in the system.
%% When this is true, it will update the user credentials of this particular client.
%% error
%% couldnotbeupdated: thrown when the client could not be updated.
%% param
%% Username: Username of the client.
%% SecretHash: Secret Hash of the client.
%% PublicKey: Public key of the client.
%% DedicatedNodes: Dedicated nodes associated with the client.
update_client(Username, SecretHash, PublicKey, DedicatedNodes) when is_list(Username)
    andalso (undefined == SecretHash orelse is_list(SecretHash))
    andalso (undefined == PublicKey orelse is_binary(PublicKey))
    andalso is_list(DedicatedNodes) ->
    case mnesia:transaction(fun() ->
        [Client] = mnesia:wread({client, Username}),
        mnesia:write(
            Client#client{username = Username,
                secrethash = SecretHash,
                publickey = PublicKey,
                dedicatednodes = DedicatedNodes})
                            end) of
        {atomic, ok} ->
            ok;
        _ ->
            error(couldnotbeupdated)
    end.

-spec update_client_hash(list(), any()) -> atom().
%% @doc
%% it will check if this client is in the system,
%% when this is true, it will update the secret hash of this particular client.
%% error
%% couldnotbeupdated: thrown when the client could not be updated.
%% param
%% Username: Username of the client.
%% SecretHash: New secret hash for the client.
update_client_hash(Username, SecretHash) when
    is_list(Username)
    andalso (undefined == SecretHash orelse is_list(SecretHash)) ->
    case mnesia:transaction(fun() ->
        [Client] = mnesia:wread({client, Username}),
        mnesia:write(
            Client#client{username = Username,
                secrethash = SecretHash})
        end) of
            {atomic, ok} ->
                ok;
            _ ->
                error(couldnotbeupdated)
    end.

-spec select_client(list()) -> any().
%% @doc
%% Given the username of a client, this method checks if there's a client in the system that matches the username.
%% If so it will return that client's credentials. Else it is undefined.
%% param
%% Username: Username of the client.
select_client(Username) when is_list(Username) ->
    case mnesia:dirty_read({client, Username}) of
        [] ->
            undefined;
        [{_, Username, SecretHash, PublicKey, Password, DedicatedNodes}] ->
            {Username, SecretHash, PublicKey, Password, DedicatedNodes}
    end.

-spec select_admin(list()) -> any().
%% @doc
%% Given the username of an admin, this method checks if there's an admin in the system that matches the username.
%% If so it will return the admin's credentials. Else it is undefined.
%% param
%% Username: Username of the admin.
select_admin(Username) when is_list(Username) ->
    case mnesia:dirty_read({admin, Username}) of
        [] ->
            undefined;
        [{_, Username, Password, SuperAdmin}] ->
            {Username, Password, SuperAdmin}
    end.

-spec select_all_admins() -> list().
%% @doc
%% When invoked, this method will return all admins that are in the system.
select_all_admins() ->
    {_, Result} = get_all_records_from_table(admin),
    [{Username, Superadmin} ||
        {_, Username, _, Superadmin} <- Result].

-spec select_all_admins_including_passwords() -> list().
%% @doc
%% When invoked, this method will return all admins that are in the system, including their password
%% and their superadmin status.
select_all_admins_including_passwords() ->
    {_, Result} = get_all_records_from_table(admin),
    [{Username, Password, Superadmin} ||
        {_, Username, Password, Superadmin} <- Result].

-spec select_all_super_admins() -> list().
%% @doc
%% When invoked, this method will return all super admins that are in the system with their passwords
%% and their superadmin status.
select_all_super_admins() ->
    Result = mnesia:dirty_match_object({admin, '_', '_', true}),
    [{Username, SuperAdmin} ||
        {_, Username, _, SuperAdmin} <- Result].

-spec insert_admin(list()) -> any().
%% @doc
%% Given a username, the method checks if the admin already exist in the system.
%% If this is not the case the admin will be added to the system.
%% A random password will be generated for this admin which can be changed later.
%% error
%% usernametaken: Thrown when a username is already present in amnesia.
%% couldnotbeinserted: Thrown when the admin could not be inserted for another reason than usernametaken.
%% param
%% Username: Username of the admin to insert.
insert_admin(Username) when is_list(Username) ->
    try
        undefined = select_admin(Username)
    catch
        _:{badmatch,_} ->
            error(usernametaken)
    end,

    case mnesia:transaction(fun() ->
        mnesia:write(
            #admin{username = Username,
                password = generate_password(),
                superadmin = false})
                            end) of
        {atomic, ok} ->
            ok;
        _ ->
            error(couldnotbeinserted)
    end.

-spec update_admin(list(), list(), boolean(), boolean()) -> any().
%% @doc
%% Depending on the given parameters the method will decide which version of the update_admin method it will call upon.
%% If the password is reset, it will only generate a new password,
%% if the password is empty or undefined, it will keep the password as it is,
%% superadmin status is changed as it is provided if the password wasn't reset.
%% param
%% Username: Username of the admin to update.
%% Password: new password of the admin.
%% Superadmin: Determines whether the admin is a superadmin.
%% _: Determines whether the password should be reset.
update_admin(Username, _Password, _SuperAdmin, true)
    when
        is_list(Username)
    ->
    {_, _, SuperAdmin} = select_admin(Username),
    update_admin(Username, generate_password(), SuperAdmin);
update_admin(Username, undefined , SuperAdmin, false)
    when
        is_list(Username), is_boolean(SuperAdmin)
    ->
    update_admin_with_known_password(Username, SuperAdmin);
update_admin(Username, "" , SuperAdmin, false)
    when
        is_list(Username), is_boolean(SuperAdmin)
    ->
    update_admin_with_known_password(Username, SuperAdmin);
update_admin(Username, Password, SuperAdmin, false)
    when
        is_list(Username), is_list(Password), is_boolean(SuperAdmin)
    ->
    update_admin(Username, Password, SuperAdmin).

-spec update_admin(list(), list(), atom()) -> any().
%% @doc
%% It will check if there will remain at least one super admin after this update
%% and if the credentials match an admin that's already in the system.
%% It will then check whether a superadmin still remains after performing the update,
%% if this is not the case, an error is thrown.
%% When this is also true, it will update the user credentials of this admin depending on what parameters are given.
%% error
%% couldnotbeupdated: thrown when an admin could not be updated.
%% invalidpassword: thrown when an illegal password has been passed.
%% noremainingsuperadmin: thrown when the client could not be updated.
%% nonexistingadmin: thrown when the passed admin does not exist.
%% param
%% Username: Username of the admin.
%% Password: Password of the admin.
%% Superadmin: Determines whether the admin is a superadmin.
update_admin(Username, Password, SuperAdmin)
    when
        is_list(Username), is_list(Password), is_atom(SuperAdmin)
    ->
    verify_super_admin_remains_after_update(Username, SuperAdmin),
    verify_valid_admin_password(Password),

    case mnesia:transaction(fun() ->
        [Admin] = mnesia:wread({admin, Username}),
        mnesia:write(
            Admin#admin{username = Username,
                password = Password,
                superadmin = SuperAdmin})
                            end) of
        {atomic, ok} ->
            ok;
        _ ->
            error(couldnotbeupdated)
    end.

-spec update_admin_with_known_password(list(), atom()) -> any().
update_admin_with_known_password(Username, SuperAdmin)
    when
        is_list(Username), is_atom(SuperAdmin)
    ->
    {_, Password, _} = select_admin(Username),
    update_admin(Username, Password, SuperAdmin).

-spec delete_admin(list()) -> atom().
%% @doc
%% Removes admin from system. First checks if there will be at least one super admin left after
%% this particular admin will be deleted, then removes the admin.
%% if no super admins remain after the deletion, an error is thrown.
%% error
%% noremainingsuperadmin: thrown when the client could not be updated.
%% nonexistingadmin: thrown when the passed admin does not exist.
%% param
%% Username: Username of the admin to delete.
delete_admin(Username)
    when
        is_list(Username)
    ->
    verify_super_admin_remains_after_delete(Username),
    mnesia:dirty_delete({admin, Username}).

-spec select_clients_by_hash(list()) -> list().
%% @doc
%% Given the secret hash of a client, this method checks if there's a client in the system that matches the secret hash.
%% If so, it will return the client's credentials. Else it will return an empty list.
%% param
%% SecretHash: Hash to select clients by.
select_clients_by_hash(SecretHash)
    when
        (undefined == SecretHash orelse is_list(SecretHash))
    ->
    Result = mnesia:dirty_match_object({client, '_', SecretHash, '_', '_', '_'}),
    [{Username, Hash, PublicKey, Password, DedicatedNodes} ||
        {_, Username, Hash, PublicKey, Password, DedicatedNodes} <- Result].

-spec select_all_clients() -> list().
%% @doc
%% When invoked, this method will return all clients that are in the database.
select_all_clients() ->
    {_, Result} = get_all_records_from_table(client),
    [{Username, SecretHash, PublicKey, Password, DedicatedNodes} ||
        {_, Username, SecretHash, PublicKey, Password, DedicatedNodes} <- Result].

-spec delete_client(list()) -> atom().
%% @doc
%% Removes client from database.
%% param
%% Username: Username of the client to delete.
delete_client(Username) when is_list(Username) ->
    mnesia:dirty_delete({client, Username}).

-spec delete_all_clients() -> atom().
%% @doc
%% Removes all clients from database. Used for testing purposes.
delete_all_clients() ->
    {_, Result} = mnesia:clear_table(client),
    Result.

-spec delete_all_admins() -> atom().
%% @doc
%% Removes all admins from database. Used for testing purposes.
delete_all_admins() ->
    {_, Result} = mnesia:clear_table(admin),
    Result.

-spec get_all_records_from_table(atom()) -> any().
get_all_records_from_table(Table) when is_atom(Table) ->
    mnesia:transaction(fun() ->
        qlc:eval(qlc:q(
            [ X || X <- mnesia:table(Table) ]
        ))
    end).

-spec generate_password() -> list().
generate_password() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(8))).

-spec verify_super_admin_remains_after_delete(list()) -> any().
verify_super_admin_remains_after_delete(Username) ->
    try
        {Username, _, SuperAdmin} = select_admin(Username),
        true = ((1 < length(select_all_super_admins()) orelse SuperAdmin == false))
    catch
        _:{badmatch, false} ->
            error(noremainingsuperadmin);
        _:_ ->
            error(nonexistingadmin)
    end.

-spec verify_super_admin_remains_after_update(list(), boolean()) -> any().
verify_super_admin_remains_after_update(Username, NewSuperAdmin) ->
    try
        {Username, _, SuperAdmin} = select_admin(Username),
        true = ((1 < length(select_all_super_admins()) orelse
            (SuperAdmin == false orelse NewSuperAdmin == true)))
    catch
        _:{badmatch, false} ->
            error(noremainingsuperadmin);
        _:_ ->
            error(nonexistingadmin)
    end.

-spec verify_valid_admin_password(list()) -> any().
verify_valid_admin_password(Password) ->
    try
        true = 4 < length(Password)
    catch
        _:_ ->
            error(invalidpassword)
    end.