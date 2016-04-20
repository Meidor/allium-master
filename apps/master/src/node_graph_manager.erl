%%%-------------------------------------------------------------------
%% @doc graph public API
%% @end
%%%-------------------------------------------------------------------
-module(node_graph_manager).

-export([get_graph_updates/1, rebuild_graph/0, rebuild_graph_at_interval/1, build_graph/1, merge_update_with_graph/2, add_node/3, remove_node/1, get_node_secret_hash/1]).

get_graph_updates(Version) when is_integer(Version) ->
    RequestedVersion = max(Version, get_min_version() - 1),
    GetMaxVersion = get_max_version(),
    case RequestedVersion of
        GetMaxVersion ->
            [];
        _ ->
            get_graph_updates_for_versions(
                get_version_numbers_since(
                    RequestedVersion
                )
            )
    end.

get_version_numbers_since(Version) ->
    lists:seq(Version + 1, get_max_version()).

get_min_version() ->
    binary_to_integer(redis:get("min_version")).

get_max_version() ->
    binary_to_integer(redis:get("max_version")).

get_graph_updates_for_versions(Versions) ->
    lists:map(
        fun(Version) -> get_graph_updates_for_version(Version) end,
        Versions
    ).

get_graph_updates_for_version(Version) ->
    redis:get("version_" ++ integer_to_list(Version)).






rebuild_graph() ->
    NewMinVersion = get_new_min_version(),
    Graph = build_graph(NewMinVersion),
    save_graph(Graph, NewMinVersion),
    remove_old_versions(NewMinVersion),
    update_min_version(NewMinVersion).

build_graph(NewMinVersion) ->
    GraphUpdates = lists:takewhile(
        fun({graphupdate, VersionNumber, _, _, _, _}) -> VersionNumber =< NewMinVersion end,
        protobuf_list_to_tuple_list(get_graph_updates(get_min_version()))
    ),
    NewGraph = lists:foldl(
        fun(Update, Graph) -> merge_update_with_graph(Update, Graph) end,
        get_current_full_graph(),
        GraphUpdates
    ),
    {graphupdate, _, _, Added, Edited, Deleted} = NewGraph,
    {graphupdate, NewMinVersion, true, Added, Edited, Deleted}.

get_current_full_graph() ->
    protobufs_to_tuple(
        get_graph_updates_for_version(
          get_min_version()
        )
    ).

merge_update_with_graph(Update, Graph) ->
    {_, _, _, ResultingAdditions, _,  _} = Graph,
    {_, _, _, Additions, _, Deletes} = Update,
    NewAdditions = ResultingAdditions ++ Additions,
    TotalDeletionsAdditions = lists:foldl(
        fun({_, DeletedNodeId, _, _, _, _}, TotalAdditions) ->
            lists:filter(
                fun({_, NodeId, _, _, _, _}) -> NodeId =/= DeletedNodeId end,
                TotalAdditions
            )
        end,
        NewAdditions,
        Deletes
    ),
    {graphupdate, 0, true, TotalDeletionsAdditions, [], []}.

update_min_version(NewMinVersion) ->
    redis:set("min_version", NewMinVersion).

get_new_min_version() ->
    round((get_min_version() + get_max_version()) / 2),
    12.

save_graph(Graph, NewMinVersion) ->
    redis:set("version_" ++ integer_to_list(NewMinVersion), Graph).

remove_old_versions(NewMinVersion) ->
    lists:map(
        fun(Version) -> redis:remove("version_" ++ integer_to_list(Version)) end,
        lists:seq(get_min_version(), NewMinVersion - 1)
    ).

protobuf_list_to_tuple_list(List) ->
    lists:map(
        fun(Item) -> protobufs_to_tuple(Item) end,
        List
    ).

protobufs_to_tuple(Data) ->
    hrp_pb:decode_graphupdate(Data).




%% Periodically rebuild the graph.
rebuild_graph_at_interval(Interval) ->
    timer:sleep(Interval),
    rebuild_graph(),
    rebuild_graph_at_interval(Interval).


add_node(IPaddress, Port, PublicKey) ->
    NodeId = base64:encode_to_string(list_to_binary(PublicKey)),
    case redis:get("node_hash_" ++ NodeId) of
        undefined ->
            error(alreadyexists);
        _ ->
            Hash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
            redis:set("node_hash_" ++ NodeId, Hash),
            Version = get_max_version() + 1,
            set_max_version(Version),
            redis:set(
                "version_" ++ integer_to_list(Version),
                hrp_pb:encode(
                    {graphupdate, Version, false, [{node, NodeId, IPaddress, Port, PublicKey, []}], [], []}
                )
            ),
            {NodeId, Hash}
    end.

remove_node(NodeId) ->
    redis:remove("node_hash_" ++ NodeId),
    Version = get_max_version() + 1,
    set_max_version(Version),
    Update = hrp_pb:encode(
        {graphupdate, Version, false, [], [], [{node, NodeId, "", 0, "", []}]}
    ),
    redis:set("version_" ++ integer_to_list(Version), Update),
    ok.

set_max_version(Version) ->
    redis:set("max_version", Version).

get_node_secret_hash(NodeId) ->
    redis:get("node_hash_" ++ NodeId).

update_node(NodeId, IPaddress, Port, PublicKey) ->
    ok.