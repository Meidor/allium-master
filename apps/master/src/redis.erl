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
    get_list_failsafe/1,
    set_randmember/2,
    set_add/2,
    set_remove/2,
    init/0,
    apply_to_matching_keys/2,
    apply_to_execute_command_on_all_nodes/2
]).

-define(prefix, "onion_").

%% @doc
%% Starts redis.
-spec init() -> any().
init() ->
    sharded_eredis:start().

-spec get(list()) -> binary().
get(Key) ->
    {ok, Value} = sharded_eredis:q(["GET", ?prefix ++ Key]),
    Value.

%% @doc
%% returns all keys matching the provided key.
-spec get_matching_keys(list()) -> list().
get_matching_keys(Key) ->
    accumulate_command_on_all_nodes(["KEYS", ?prefix ++ Key ++ "*"]).

%% @doc
%% Gets a list of nodes based on a list of keys or retuns an empty list if no keys are passed.
-spec get_list(list()) -> list().
get_list([])->
    [];
get_list(ListOfKeys) ->
    {ok, ListOfValues} = sharded_eredis:q(["MGET" | ListOfKeys]),
    ListOfValues.

%% @doc
%% Used for testing purposes.
-spec get_list_failsafe(list()) -> list().
get_list_failsafe(ListOfKeys) ->
    lists:map(
        fun(Key) -> {ok, Value} = sharded_eredis:q(["GET", Key]), Value end,
        ListOfKeys
    ).

%% @doc
%% Sets a key-value pair in redis.
-spec set(list(), list()) -> tuple().
set(Key, Value) ->
    sharded_eredis:q(["SET", ?prefix ++ Key, Value]).

%% @doc
%% Removes a key-value pair from redis.
-spec remove(list()) -> tuple().
remove(Key) ->
    sharded_eredis:q(["DEL", ?prefix ++ Key]).

%% @doc
%% Returns an amount of random nodes.
-spec set_randmember(list(), integer()) -> list().
set_randmember(Set, Amount) ->
    {ok, Keys} = sharded_eredis:q(["SRANDMEMBER", ?prefix ++ Set,  Amount]),
    Keys.

%% @doc
%% Adds the value to a set.
-spec set_add(list(), list()) -> tuple().
set_add(Set, Value) ->
    sharded_eredis:q(["SADD", ?prefix ++ Set, Value]).

%% @doc
%% Removes a value from a specified set.
-spec set_remove(list(), list()) -> tuple().
set_remove(Set, Value) ->
    sharded_eredis:q(["SREM", ?prefix ++ Set, Value]).

%% @doc
%% Performs a function for all keys matching a filter.
-spec apply_to_matching_keys(list(), fun()) -> atom().
apply_to_matching_keys(Filter, Fun) ->
    apply_to_execute_command_on_all_nodes(["KEYS", ?prefix ++ Filter ++ "*"], Fun).

%% @doc
%% Applies a function to each node.???          todo Raoul vragen.
-spec apply_to_execute_command_on_all_nodes(list(), fun()) -> atom().
apply_to_execute_command_on_all_nodes(Command, Fun) ->
    {ok, NodeList} = application:get_env(sharded_eredis, ring),
    Nodes = [Node || {_, Node} <- NodeList],
    lists:foreach(
        fun(Node) ->
            {ok, Response} = sharded_eredis:q2(Node, Command),
            Fun(Response)
        end,
        Nodes
    ),
    ok.

-spec accumulate_command_on_all_nodes(list()) -> list().
accumulate_command_on_all_nodes(Command) ->
    {ok, NodeList} = application:get_env(sharded_eredis, ring),
    Nodes = [Node || {_, Node} <- NodeList],
    lists:foldl(
        fun(Node, ResponseAcc) ->
            {ok, Response} = sharded_eredis:q2(Node, Command),
            ResponseAcc ++ Response
        end,
        [],
        Nodes
    ).
