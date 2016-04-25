-module(heartbeat_monitor).

%% API
-export([receive_heartbeat/2,
    remove_inactive_nodes/1,
    add_node/1,
    remove_node/1,
    get_current_time/0]).

-spec receive_heartbeat(list(), list()) -> any().
receive_heartbeat(NodeId, SecretHash) when is_list(NodeId), is_list(SecretHash) ->
    try
        node_service:verify_node(NodeId, SecretHash)
    of _ ->
        redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()),
        ok
    catch _:_ ->
        {error, "Node id and secret hash do not match"}
    end.

-spec remove_inactive_nodes(integer()) -> list().
remove_inactive_nodes(TimeBetweenHeartbeats) when is_integer(TimeBetweenHeartbeats) ->
    AllKeys = [binary_to_list(Key) || Key <- redis:get_matching_keys("heartbeat_node_")],
    AllValues = [binary_to_integer(Value) || Value <- redis:get_list(AllKeys)],
    ExpiredNodes = [Key || {Key, Value} <- lists:zip(AllKeys, AllValues), Value < (?MODULE:get_current_time() - TimeBetweenHeartbeats)],
    LengthOfLabel = 22, %Length of "onion_heartbeat_node_", Key consists of Label + NodeId
    ExperidNodeIds = [string:substr(Key, LengthOfLabel) || Key <- ExpiredNodes],
    lists:foreach(
        fun(Node) ->
            node_service:node_unregister(Node),
            remove_node(Node)
        end,
        ExperidNodeIds),
    ExperidNodeIds.

-spec add_node(list()) -> atom().
add_node(NodeId) when is_list(NodeId) ->
    redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()),
    ok.

-spec remove_node(list()) -> atom().
remove_node(NodeId) when is_list(NodeId) ->
    redis:remove("heartbeat_node_" ++ NodeId),
    ok.

-spec get_current_time() -> integer().
get_current_time() ->
    {_, CurrentTime, _} = erlang:timestamp(),
    CurrentTime.