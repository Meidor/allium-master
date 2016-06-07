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
%% @doc
%% Handles receiving a heartbeat from a node. First verifies the node,
%% then registers the time the last heartbeat
%% has been recieved for that node.
%% params
%% NodeId: Id of the node.
%% SecretHash: Secret hash of the node.
%% error
%% nodenotverified: When the NodeId and the Secrethash combination do not match the known values.
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
%% @doc
%% Handles receiving a heartbeat from a client. First verifies the client,
%% then registers the time the last heartbeat
%% has been recieved for that client.
%% params
%% Username: The username of the client.
%% SecretHash: The secret hash of the client.
%% error
%% clientnotverified: When the combination of username and secrethash don't match the known values.
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
%% @doc
%% Checks for all the nodes that haven't passed a heartbeat
%% and removes them from the graph and from the heartbeat_monitor.
%% params
%% TimeBetweenHeartbeats: the time between two heartbeats.
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
%% @doc
%% Checks for all the clients that haven't passed a heartbeat. then logs out those clients.
%% params
%% TimeBetweenHeartbeats: the time between two heartbeats.
remove_inactive_clients(TimeBetweenHeartbeats) when is_integer(TimeBetweenHeartbeats) ->
    apply_to_expired_heartbeats(
        "heartbeat_client_",
        TimeBetweenHeartbeats,
        fun(Client) ->
            client_service:client_logout(Client)
        end
    ).

-spec add_node(list()) -> atom().
%% @doc
%% Saves the time the heartbeat has been received from the node.
%% params
%% NodeId: Id of the node.
add_node(NodeId) when is_list(NodeId) ->
    redis:set("heartbeat_node_" ++ NodeId, ?MODULE:get_current_time()).

-spec add_client(list()) -> atom().
%% @doc
%% Saves the time the heartbeat has been received from the client.
%% params
%% Username: username of the client.
add_client(Username) when is_list(Username) ->
    redis:set("heartbeat_client_" ++ Username, ?MODULE:get_current_time()).

-spec remove_node(list()) -> atom().
%% @doc
%% Removes a node from the nodes to check for a heartbeat.
%% params
%% NodeId: Id of the node.
remove_node(NodeId) when is_list(NodeId) ->
    redis:remove("heartbeat_node_" ++ NodeId).

-spec remove_client(list()) -> atom().
%% @doc
%% Removes a client from the clients to check for a heartbeat.
%% params
%% Username: The username of the client.
remove_client(Username) when is_list(Username) ->
    redis:remove("heartbeat_client_" ++ Username).

-spec get_current_time() -> integer().
%% @doc
%% returns a timestamp representing the current time.
get_current_time() ->
    {Mega, Secs, _} = erlang:timestamp(),
    Mega * 1000000 + Secs.

-spec apply_to_expired_heartbeats(list(), integer(), fun()) -> list().
apply_to_expired_heartbeats(Label, TimeBetweenHeartbeats, Fun) ->
    redis:apply_to_matching_keys(
        Label,
        fun(Keys) ->
            AllValues = [binary_to_integer(Value) || Value <- redis:get_list(Keys)],
            LengthOfLabel = length("onion_") + length(Label),
            lists:foreach(
                Fun,
                [string:substr(binary_to_list(Key), LengthOfLabel + 1) ||
                    {Key, Value} <- lists:zip(Keys, AllValues),
                    Value < (?MODULE:get_current_time() - TimeBetweenHeartbeats)]
            )
        end
    ).
