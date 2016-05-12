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
    {NodeId, SecretHash} = node_graph_manager:add_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:add_node(NodeId),
    {NodeId, SecretHash}.

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

%% @doc Update a node
%% @end
-spec node_update(list(), list(), list(), integer(), binary()) -> any().
node_update(NodeId, SecretHash, IPaddress, Port, PublicKey)
    when
        is_list(NodeId)
        andalso is_list(SecretHash)
        andalso (undefined == IPaddress orelse is_list(IPaddress))
        andalso (undefined == Port orelse (is_integer(Port) andalso Port > 0 andalso Port < 65536))
        andalso (is_binary(PublicKey) orelse undefined == PublicKey)
    ->
    node_verify(NodeId, SecretHash),
    node_graph_manager:update_node(NodeId, IPaddress, Port, PublicKey).