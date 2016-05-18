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
    get_list/1,
    set_randmember/2,
    set_add/2,
    set_remove/2,
    execute_command_on_all_nodes/1,
    init/0]).

-define(prefix, "onion_").

init() ->
    sharded_eredis:start().

-spec get(list()) -> list().
get(Key) ->
    {ok, Value} = sharded_eredis:q(["GET", ?prefix ++ Key]),
    Value.

-spec get_matching_keys(list()) -> list().
get_matching_keys(Key) ->
    {ok, Keys} = eredis:q(get_connection(), ["KEYS", ?prefix ++ Key ++ "*"]),
    Keys.

-spec get_list(list()) -> list().
get_list([])->
    [];
get_list(ListOfKeys) ->
    {ok, ListOfValues} = eredis:q(get_connection(), ["MGET" | ListOfKeys]),
    ListOfValues.

-spec set(list(), list()) -> any().
set(Key, Value) ->
    sharded_eredis:q(["SET", ?prefix ++ Key, Value]).

-spec remove(list()) -> any().
remove(Key) ->
    sharded_eredis:q(["DEL", ?prefix ++ Key]).

-spec set_randmember(list(), integer()) -> any().
set_randmember(Set, Amount) ->
    {ok, Keys} = sharded_eredis:q(["SRANDMEMBER", ?prefix ++ Set,  Amount]),
    Keys.

-spec set_add(list(), list()) -> any().
set_add(Set, Value) ->
    sharded_eredis:q(["SADD", ?prefix ++ Set, Value]).

-spec set_remove(list(), list()) -> any().
set_remove(Set, Value) ->
    sharded_eredis:q(["SREM", ?prefix ++ Set, Value]).

-spec get_connection() -> pid().
get_connection() ->
    case whereis(redis) of
        undefined ->
            {ok, Connection} = eredis:start_link(),
            register(redis, Connection),
            Connection;
        Pid ->
            Pid
    end.

execute_command_on_all_nodes(Command) ->
    {ok, NodeList} = application:get_env(sharded_eredis, ring),
    Nodes = [Node || {_, Node} <- NodeList],
    lists:map(
        fun(Node) -> {ok, Response} = sharded_eredis:q2(Node, Command), Response end,
        Nodes
    ).
