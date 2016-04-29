-module(persistence_service).

-export([
    init/0,
    insert_client/4,
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

-spec insert_client(list(), list(), list(), list()) -> atom().
insert_client(Username, SecretHash, PublicKey, Password) ->
    mnesia:transaction(fun() ->
        mnesia:write(
            #client{username = Username,
                secrethash = SecretHash,
                publickey = PublicKey,
                password = Password} )
        end),
    ok.

-spec select_client(list()) -> any().
select_client(Username) ->
    {_, Result} = mnesia:transaction(fun() ->
        mnesia:read({client, Username})
        end),
    case Result of
        [] ->
            undefined;
        [{_, Username, SecretHash, PublicKey, Password}] ->
            {Username, SecretHash, PublicKey, Password}
    end.

-spec select_clients_by_hash(list()) -> list().
select_clients_by_hash(Hash) ->
    {_, Result} = mnesia:transaction(fun() ->
        mnesia:match_object({client, '_', Hash, '_', '_'})
        end),
    [{Username, SecretHash, PublicKey, Password} ||
        {_, Username, SecretHash, PublicKey, Password} <- Result].

-spec select_all_clients() -> list().
select_all_clients() ->
    {_, Result} = get_all_records_from_table(client),
    [{Username, SecretHash, PublicKey, Password} ||
        {_, Username, SecretHash, PublicKey, Password} <- Result].

-spec delete_client(list()) -> atom().
delete_client(Username) ->
    mnesia:transaction(fun() ->
        mnesia:delete({client, Username})
    end),
    ok.

-spec delete_all_clients() -> atom().
delete_all_clients() ->
    mnesia:clear_table(client),
    ok.

-spec get_all_records_from_table(atom()) -> any().
get_all_records_from_table(Table) ->
    mnesia:transaction(fun() ->
        qlc:eval(qlc:q(
            [ X || X <- mnesia:table(Table) ]
        ))
    end).