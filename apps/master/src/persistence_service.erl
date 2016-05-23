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
    select_admin/1
]).

-include_lib("stdlib/include/qlc.hrl").

-record(client, {username, secrethash, publickey, password, dedicatednodes = []}).

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
                record_info(fields, admin)}]).

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

-spec get_all_records_from_table(atom()) -> any().
get_all_records_from_table(Table) when is_atom(Table) ->
    mnesia:transaction(fun() ->
        qlc:eval(qlc:q(
            [ X || X <- mnesia:table(Table) ]
        ))
    end).