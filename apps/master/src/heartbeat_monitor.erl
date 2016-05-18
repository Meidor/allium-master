-module(heartbeat_monitor).

%% API
-export([
    receive_heartbeat_node/2,
    receive_heartbeat_client/2,
    remove_inactive_nodes/1,
    remove_inactive_clients/1,
    add_node/1,
    add_client/1,
    remove_node/1,
    remove_client/1,
    get_current_time/0]).

-spec receive_heartbeat_node(list(), list()) -> any().
receive_heartbeat_node(NodeId, SecretHash) when is_list(NodeId), is_list(SecretHash) ->
    try
        node_service:node_verify(NodeId, SecretHash)
    of _ ->
        redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time())
    catch
        _:_ ->
            error(nodenotverified)
    end.

-spec receive_heartbeat_client(list(), list()) -> any().
receive_heartbeat_client(Username, SecretHash) when is_list(Username), is_list(SecretHash) ->
    try
        client_service:client_verify(Username, SecretHash)
    of _ ->
        redis:set("heartbeat_client_" ++ Username, ?MODULE:get_current_time())
    catch
        _:_ ->
            error(clientnotverified)
    end.


-spec remove_inactive_nodes(integer()) -> list().
remove_inactive_nodes(TimeBetweenHeartbeats) when is_integer(TimeBetweenHeartbeats) ->
    apply_to_expired_heartbeats(
        "heartbeat_node_",
        TimeBetweenHeartbeats,
        fun(Node) ->
            node_service:node_unregister(Node),
            remove_node(Node)
        end
    ).

-spec remove_inactive_clients(integer()) -> list().
remove_inactive_clients(TimeBetweenHeartbeats) when is_integer(TimeBetweenHeartbeats) ->
    apply_to_expired_heartbeats(
        "heartbeat_client_",
        TimeBetweenHeartbeats,
        fun(Client) ->
            client_service:client_logout(Client),
            remove_client(Client)
        end
    ).

-spec add_node(list()) -> atom().
add_node(NodeId) when is_list(NodeId) ->
    redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()).

-spec add_client(list()) -> atom().
add_client(Username) when is_list(Username) ->
    redis:set("heartbeat_client_" ++ Username, ?MODULE:get_current_time()).

-spec remove_node(list()) -> atom().
remove_node(NodeId) when is_list(NodeId) ->
    redis:remove("heartbeat_node_" ++ NodeId).

-spec remove_client(list()) -> atom().
remove_client(Username) when is_list(Username) ->
    redis:remove("heartbeat_client_" ++ Username).

-spec get_current_time() -> integer().
get_current_time() ->
    {_, CurrentTime, _} = erlang:timestamp(),
    CurrentTime.

-spec apply_to_expired_heartbeats(list(), integer(), fun()) -> list().
apply_to_expired_heartbeats(Label, TimeBetweenHeartbeats, Fun) ->
    redis:apply_to_matching_keys(
        Label,
        fun(Keys) ->
            AllValues = [binary_to_integer(Value) || Value <- redis:get_list(Keys)],
            LengthOfLabel = length("onion_") + length(Label),
            lists:foreach(
                Fun,
                [string:substr(Key, LengthOfLabel + 1) ||
                    {Key, Value} <- lists:zip(Keys, AllValues),
                    Value < (?MODULE:get_current_time() - TimeBetweenHeartbeats)]
            )
        end
    ).
