%%%===================================================================
%% @doc master public API
%% @end
%%%===================================================================
-module(node_service).
-export([node_register/3,
    node_unregister/1, node_unregister/2,
    node_verify/2,
    node_update/5
]).

%% @doc Register your node in the graph
%% @end
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

%% @doc Unregister your node in the graph
%% @end
-spec node_unregister(list()) -> any().
node_unregister(NodeId)
    when
        is_list(NodeId)
    ->
    node_graph_manager:remove_node(NodeId).

-spec node_unregister(list(), list()) -> any().
node_unregister(NodeId, SecretHash)
    when
        is_list(NodeId), is_list(SecretHash)
    ->
    node_verify(NodeId, SecretHash),
    node_graph_manager:remove_node(NodeId),
    heartbeat_monitor:remove_node(NodeId).

%% @doc Verify the secrethash of a node
%% @end
-spec node_verify(list(), list()) -> list().
node_verify(NodeId, SecretHash)
    when
        is_list(NodeId), is_list(SecretHash)
    ->
    SecretHash = node_graph_manager:get_node_secret_hash(NodeId).

-spec get_edges(list()) -> list().
get_edges(NodeId) when is_list(NodeId) ->
    ok.
    %EncodedEdges = redis:get(NodeId);
    %Edges = decode(EncodedEges);

%% @doc Update a node
%% @end
-spec node_update(list(), list(), list(), integer(), binary()) -> any().
node_update(NodeId, SecretHash, IPaddress, Port, PublicKey)
    when
        is_list(NodeId),
        is_list(SecretHash),
        is_list(IPaddress),
        is_integer(Port),
        Port > 0,
        Port < 65536,
        is_binary(PublicKey)
    ->
    verify_ip(IPaddress),
    node_verify(NodeId, SecretHash),
    Edges = get_edges(NodeId),
    case NodeId =:= IPaddress ++ integer_to_list(Port) of
        true ->
            node_graph_manager:update_node(NodeId, IPaddress, Port, PublicKey, Edges),
            NodeId;
        _ ->
            node_unregister(NodeId, SecretHash),
            {NodeId, _} = node_register(IPaddress, Port, PublicKey),
            redis:set("node_hash_" ++ NodeId, SecretHash),
            
            NodeId
    end.
