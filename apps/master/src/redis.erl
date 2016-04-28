%%%===================================================================
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% @end
%%% Created : 18. Apr 2016 10:25
%%%===================================================================
-module(redis).

%% API
-export([get/1,
    set/2,
    remove/1,
    get_matching_keys/1,
    get_list/1]).

-spec get(list()) -> list().
get(Key) ->
    {ok, Connection} = eredis:start_link(),
    {ok, Value} = eredis:q(Connection, ["GET", "onion_" ++ Key]),
    Value.

-spec get_matching_keys(list()) -> list().
get_matching_keys(Key) ->
    {ok, Connection} = eredis:start_link(),
    {ok, Keys} = eredis:q(Connection, ["KEYS", "onion_" ++ Key ++ "*"]),
    Keys.

-spec get_list(list()) -> list().
get_list([])->
    [];
get_list(ListOfKeys) ->
    {ok, Connection} = eredis:start_link(),
    {ok, ListOfValues} = eredis:q(Connection, ["MGET" | ListOfKeys]),
    ListOfValues.

-spec set(list(), list()) -> any().
set(Key, Value) ->
    {ok, Connection} = eredis:start_link(),
    eredis:q(Connection, ["SET", "onion_" ++ Key, Value]).

-spec remove(list()) -> any().
remove(Key) ->
    {ok, Connection} = eredis:start_link(),
    eredis:q(Connection, ["DEL", "onion_" ++ Key]).