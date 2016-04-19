%%%-------------------------------------------------------------------
%% @doc Stub node graph manager
%% @end
%%%-------------------------------------------------------------------

-module(node_graph_manager).
-export([add_node/3, remove_node/1, get_node_secret_hash/1, update_node/4]).

add_node(IPaddress, Port, "AlreadyExists") ->
    error(alreadyexists);
add_node (IPadress, Port, PublicKey) ->
    {"42", "SecretHash"}.

remove_node(NodeId) -> 
    ok.

get_node_secret_hash(NodeId) ->
    "SecretHash".

update_node(NodeId, IPaddress, Port, PublicKey) ->
    ok.



