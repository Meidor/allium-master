-module(node_service).
-export([node_register/3,
    node_unregister/1, node_unregister/2,
    node_verify/2,
    node_update/5,
    node_exists/1
]).

%% @doc
%% Allows for the registration of nodes. The passed information is checked, then passed on to the node_graph_manager.
%% Also starts the heartbeat monitor for the node.
-spec node_register(list(), integer(), binary()) -> tuple().
node_register(IPaddress, Port, PublicKey)
    when
        is_list(IPaddress), is_integer(Port), Port > 0, Port < 65536, is_binary(PublicKey)
    ->
    verify_ip(IPaddress),
    {NodeId, SecretHash} = node_graph_manager:add_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:add_node(NodeId),
    {NodeId, SecretHash}.

-spec verify_ip(list()) -> tuple().
verify_ip(IPaddress) ->
    {Type, Response} = inet:parse_strict_address(IPaddress),
    case Type of
        ok -> ok;
        error -> error(Response)
    end.

%% @doc
%% Removes nodes. First checks the passed information, then removes the node. Used by the heartbeat monitor.
-spec node_unregister(list()) -> any().
node_unregister(NodeId)
    when
        is_list(NodeId)
    ->
    node_graph_manager:remove_node(NodeId).

%% @doc
%% Removes nodes. First checks the passed information, then removes the node and the accompanying heartbeat.
-spec node_unregister(list(), list()) -> any().
node_unregister(NodeId, SecretHash)
    when
        is_list(NodeId), is_list(SecretHash)
    ->
    node_verify(NodeId, SecretHash),
    node_graph_manager:remove_node(NodeId),
    heartbeat_monitor:remove_node(NodeId).

%% @doc
%% Checks whether the secret hash matches the known secret hash for the NodeId.
-spec node_verify(list(), list()) -> list().
node_verify(NodeId, SecretHash)
    when
        is_list(NodeId), is_list(SecretHash)
    ->
    SecretHash = node_graph_manager:get_node_secret_hash(NodeId).

-spec get_edges(list()) -> list().
get_edges(NodeId) when is_list(NodeId) ->
    {Edges, _} = hrp_pb:delimited_decode_edge(
        redis:get("node_edges_" ++ NodeId)
    ),
    Edges.

-spec set_edges(list(), list()) -> tuple().
set_edges(NodeId, Edges) when is_list(NodeId) ->
    redis:set(
        "node_edges_" ++ NodeId,
        hrp_pb:encode(
            Edges
        )
    ).

%% @doc
%% Updates a node in the graph. First checks the passed data, then checks whether the ipaddress or port is changed.
%% If that is the case, remove the node and add a new one with the new nodeId, otherwise, update the node.
-spec node_update(list(), list(), list(), integer(), binary()) -> any().
node_update(NodeId, SecretHash, IPaddress, Port, PublicKey)
    when
        is_list(NodeId), is_list(SecretHash), is_list(IPaddress), is_integer(Port),
        Port > 0, Port < 65536, is_binary(PublicKey)
    ->
    verify_ip(IPaddress),
    node_verify(NodeId, SecretHash),
    Edges = get_edges(NodeId),
    case NodeId =:= IPaddress ++ ":" ++ integer_to_list(Port) of
        true ->
            node_graph_manager:update_node(NodeId, IPaddress, Port, PublicKey, Edges),
            NodeId;
        _ ->
            node_unregister(NodeId, SecretHash),
            {NewNodeId, _} = node_register(IPaddress, Port, PublicKey),
            redis:set("node_hash_" ++ NewNodeId, SecretHash),
            set_edges(NewNodeId, Edges),
            NewNodeId
    end.

%% @doc
%% Checks whether a node exists in the current graph.
-spec node_exists(list()) -> any().
node_exists(NodeId) when is_list(NodeId) ->
    false = undefined == node_graph_manager:get_node_secret_hash(NodeId).
