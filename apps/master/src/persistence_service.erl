-module(persistence_service).

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
init() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(client,
        [ {disc_copies, [node()] },
            {attributes,
                record_info(fields, client)}]),
    mnesia:create_table(admin,
        [ {disc_copies, [node()]},
            {attributes,
                record_info(fields, admin)}]),

    case length(select_all_super_admins()) of
        0 ->
            insert_admin("admin"),
            update_admin("admin", "password", true, false);
        _ ->
            ok
    end.

-spec insert_client(list(), list()) -> atom().
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
select_client(Username) when is_list(Username) ->
    case mnesia:dirty_read({client, Username}) of
        [] ->
            undefined;
        [{_, Username, SecretHash, PublicKey, Password, DedicatedNodes}] ->
            {Username, SecretHash, PublicKey, Password, DedicatedNodes}
    end.

-spec select_admin(list()) -> any().
select_admin(Username) when is_list(Username) ->
    case mnesia:dirty_read({admin, Username}) of
        [] ->
            undefined;
        [{_, Username, Password, SuperAdmin}] ->
            {Username, Password, SuperAdmin}
    end.

-spec select_all_admins() -> list().
select_all_admins() ->
    {_, Result} = get_all_records_from_table(admin),
    [{Username, Superadmin} ||
        {_, Username, _, Superadmin} <- Result].

-spec select_all_admins_including_passwords() -> list().
select_all_admins_including_passwords() ->
    {_, Result} = get_all_records_from_table(admin),
    [{Username, Password, Superadmin} ||
        {_, Username, Password, Superadmin} <- Result].

-spec select_all_super_admins() -> list().
select_all_super_admins() ->
    Result = mnesia:dirty_match_object({admin, '_', '_', true}),
    [{Username, SuperAdmin} ||
        {_, Username, _, SuperAdmin} <- Result].

-spec insert_admin(list()) -> any().
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

-spec update_admin(list(), list(), atom(), atom()) -> any().
update_admin(Username, _Password, SuperAdmin, true) when is_list(Username), is_atom(SuperAdmin) ->
    update_admin(Username, generate_password(), SuperAdmin);
update_admin(Username, undefined , SuperAdmin, false) when is_list(Username), is_atom(SuperAdmin) ->
    update_admin_with_known_password(Username, SuperAdmin);
update_admin(Username, "" , SuperAdmin, false) when is_list(Username), is_atom(SuperAdmin) ->
    update_admin_with_known_password(Username, SuperAdmin);
update_admin(Username, Password, SuperAdmin, false) when is_list(Username), is_list(Password), is_atom(SuperAdmin) ->
    update_admin(Username, Password, SuperAdmin).

-spec update_admin(list(), list(), atom()) -> any().
update_admin(Username, Password, SuperAdmin) when is_list(Username), is_list(Password), is_atom(SuperAdmin) ->
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
update_admin_with_known_password(Username, SuperAdmin) when is_list(Username), is_atom(SuperAdmin) ->
    {_, Password, _} = select_admin(Username),
    update_admin(Username, Password, SuperAdmin).


-spec delete_admin(list()) -> atom().
delete_admin(Username) when is_list(Username) ->
    verify_super_admin_remains_after_delete(Username),
    mnesia:dirty_delete({admin, Username}).

-spec select_clients_by_hash(list()) -> list().
select_clients_by_hash(SecretHash) when (undefined == SecretHash orelse is_list(SecretHash)) ->
    Result = mnesia:dirty_match_object({client, '_', SecretHash, '_', '_', '_'}),
    [{Username, Hash, PublicKey, Password, DedicatedNodes} ||
        {_, Username, Hash, PublicKey, Password, DedicatedNodes} <- Result].

-spec select_all_clients() -> list().
select_all_clients() ->
    {_, Result} = get_all_records_from_table(client),
    [{Username, SecretHash, PublicKey, Password, DedicatedNodes} ||
        {_, Username, SecretHash, PublicKey, Password, DedicatedNodes} <- Result].

-spec delete_client(list()) -> atom().
delete_client(Username) when is_list(Username) ->
    mnesia:dirty_delete({client, Username}).

-spec delete_all_clients() -> atom().
delete_all_clients() ->
    {_, Result} = mnesia:clear_table(client),
    Result.

-spec delete_all_admins() -> atom().
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
        true = ((1 < length(select_all_super_admins()) orelse (SuperAdmin == false orelse NewSuperAdmin == true)))
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