%%%-------------------------------------------------------------------
%% @doc Stub node graph manager
%% @end
%%%-------------------------------------------------------------------

-module(node_graph_manager).
-export([add_node/3]).

add_node (IPadress, Port, PublicKey) ->
    {"42", "SecretHash"}.

remove_node(NodeId) -> 
    ok.

get_node_secret_hash(NodeId) ->
    "SecretHash".

update_node(NodeId, IPaddress, Port, PublicKey) ->
    ok.



