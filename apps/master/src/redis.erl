-module(redis).

%% API
-export([
    get/1,
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

-spec init() -> any().
%% @doc
%% Starts eredis.
init() ->
    sharded_eredis:start().

-spec get(list()) -> binary().
%% @doc
%% Returns the node matching the provided key.
%% params
%% Key: Key of the key-value pair that you are trying to get.
get(Key) ->
    {ok, Value} = sharded_eredis:q(["GET", ?prefix ++ Key]),
    Value.

-spec get_matching_keys(list()) -> list().
%% @doc
%% returns all values matching the provided key.
%% params
%% Key: Key of the key_value pairs that you are trying to get.
get_matching_keys(Key) ->
    accumulate_command_on_all_nodes(["KEYS", ?prefix ++ Key ++ "*"]).

-spec get_list(list()) -> list().
%% @doc
%% Gets a list of nodes based on a list of keys or retuns an empty list if no keys are passed.
%% params
%% ListOfKeys: List of keys that you want to have a selection of nodes from.
get_list([])->
    [];
get_list(ListOfKeys) ->
    {ok, ListOfValues} = sharded_eredis:q(["MGET" | ListOfKeys]),
    ListOfValues.

-spec get_list_failsafe(list()) -> list().
%% @doc
%% Used for testing purposes.
%% params
%% ListOfKeys: List of keys for getting values.
get_list_failsafe(ListOfKeys) ->
    lists:map(
        fun(Key) -> {ok, Value} = sharded_eredis:q(["GET", Key]), Value end,
        ListOfKeys
    ).

-spec set(list(), list()) -> tuple().
%% @doc
%% Sets a key-value pair in redis.
%% params
%% Key: Key of the value that you are trying to set.
%% Value: Value that you are trying to set.
set(Key, Value) ->
    sharded_eredis:q(["SET", ?prefix ++ Key, Value]).

-spec remove(list()) -> tuple().
%% @doc
%% Removes a key-value pair from redis.
%% params
%% Key: Key of the key-value pair that needs to be removed
remove(Key) ->
    sharded_eredis:q(["DEL", ?prefix ++ Key]).

-spec set_randmember(list(), integer()) -> list().
%% @doc
%% Returns an amount of random nodes.
%% params
%% Set: Set to return nodes from.
%% Amount: The amount of nodes to return.
set_randmember(Set, Amount) ->
    {ok, Keys} = sharded_eredis:q(["SRANDMEMBER", ?prefix ++ Set,  Amount]),
    Keys.

%% @doc
%% Adds the value to a set.
-spec set_add(list(), list()) -> tuple().
%% @doc
%% Adds the value to a set.
%% params
%% Set: Set to add nodes to.
%% Value: Value to add to the set.
set_add(Set, Value) ->
    sharded_eredis:q(["SADD", ?prefix ++ Set, Value]).

-spec set_remove(list(), list()) -> tuple().
%% @doc
%% Removes the specified value from the specified set.
%% params
%% Set: Set to remove node from
%% Value: Value to remove from set
set_remove(Set, Value) ->
    sharded_eredis:q(["SREM", ?prefix ++ Set, Value]).

-spec apply_to_matching_keys(list(), fun()) -> atom().
%% @doc
%% Performs a function for all keys matching a filter.
%% params
%% Filter: To select wanted nodes
%% Fun: Function to execute on the nodes
apply_to_matching_keys(Filter, Fun) ->
    apply_to_execute_command_on_all_nodes(["KEYS", ?prefix ++ Filter ++ "*"], Fun).

-spec apply_to_execute_command_on_all_nodes(list(), fun()) -> atom().
%% @doc
%% Executes a command on all redisnodes.
%% params
%% Filter: To select wanted nodes
%% Fun: Function to execute on the nodes
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
