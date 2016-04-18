%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(node_service).
-export([node_register/3, node_unregister/3]).

%% @doc Register your node in the graph
%% @end
node_register(IPaddress, Port, PublicKey) when is_list(IPaddress), is_integer(Port), Port > 0, Port < 65536, is_list(PublicKey) -> 
    node_graph_manager:add_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:add_node(IPaddress, Port, PublicKey).

%% @doc Unregister your node in the graph
%% @end
node_unregister(IPaddress, Port, PublicKey) when is_list(IPaddress), is_integer(Port), Port > 0, Port < 65536, is_list(PublicKey) ->
    node_graph_manager:remove_node(IPaddress, Port, PublicKey),
    heartbeat_monitor:remove_node(IPaddress, Port, PublicKey).