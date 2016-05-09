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
    get_all_records_from_table/1
]).

-include_lib("stdlib/include/qlc.hrl").

-record(client, {username, secrethash, publickey, password}).

-spec init() -> any().
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(client,
        [ {disc_copies, [node()] },
            {attributes,
                record_info(fields, client)} ]).

-spec insert_client(list(), list()) -> atom().
insert_client(Username, Password) when is_list(Username), is_list(Password) ->
    try
        case mnesia:transaction(fun() ->
            lager:info("Inserting client: Case statement entered.."),
            undefined = select_client(Username),
            lager:info("Inserting client: Username does not already exist.."),
            mnesia:write(
                #client{username = Username,
                    password = Password})
            end) of
                {atomic, ok} ->
                    lager:info("Inserting client: Account created.."),
                    ok;
                ErrorMessage ->
                    lager:info(io_lib:format("~p", [ErrorMessage])),
                    lager:info("Inserting client: Something went wrong.."),
                    error(couldnotbeinserted)
        end
    catch
        Error:Message ->
            lager:info(Error),
            lager:info(Message),
            lager:info("Inserting client: Catch statement entered.."),
            error(usernametaken)
    end.

-spec update_client_hash(list(), list()) -> atom().
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
        [{_, Username, SecretHash, PublicKey, Password}] ->
            {Username, SecretHash, PublicKey, Password}
    end.

-spec select_clients_by_hash(list()) -> list().
select_clients_by_hash(SecretHash) when (undefined == SecretHash orelse is_list(SecretHash)) ->
    Result = mnesia:dirty_match_object({client, '_', SecretHash, '_', '_'}),
    [{Username, SecretHash, PublicKey, Password} ||
        {_, Username, SecretHash, PublicKey, Password} <- Result].

-spec select_all_clients() -> list().
select_all_clients() ->
    {_, Result} = get_all_records_from_table(client),
    [{Username, SecretHash, PublicKey, Password} ||
        {_, Username, SecretHash, PublicKey, Password} <- Result].

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