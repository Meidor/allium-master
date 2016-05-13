%%%===================================================================
%% @doc graph public API
%% @end
%%%===================================================================
-module(node_graph_manager).

-export([get_graph_updates/1,
    rebuild_graph/0,
    build_graph/1,
    merge_update_with_graph/2,
    add_node/3,
    remove_node/1,
    get_node_secret_hash/1,
    update_node/4]).

-spec get_graph_updates(integer()) -> list().
get_graph_updates(Version) when is_integer(Version) ->
    MaxVersion = get_max_version(),
    RequestedVersion = min(max(Version, get_min_version() - 1), MaxVersion),
    case RequestedVersion of
        MaxVersion ->
            [];
        _ ->
            get_graph_updates_for_versions(
                get_version_numbers_since(
                    RequestedVersion
                )
            )
    end.

-spec get_version_numbers_since(integer()) -> list().
get_version_numbers_since(Version) ->
    lists:seq(Version + 1, get_max_version()).

-spec get_min_version() -> integer().
get_min_version() ->
    try
        binary_to_integer(redis:get("min_version"))
    catch _:_ ->
        1
    end.

-spec get_max_version() -> integer().
get_max_version() ->
    try
        binary_to_integer(redis:get("max_version"))
    catch _:_ ->
        1
    end.

-spec get_graph_updates_for_versions(list()) -> list().
get_graph_updates_for_versions(Versions) ->
    lists:map(
        fun(Version) -> get_graph_updates_for_version(Version) end,
        Versions
    ).

-spec get_graph_updates_for_version(integer()) -> binary().
get_graph_updates_for_version(Version) ->
    redis:get("version_" ++ integer_to_list(Version)).


-spec rebuild_graph() -> atom().
rebuild_graph() ->
    NewMinVersion = get_new_min_version(),
    case NewMinVersion of
        1 ->
            Graph = {graphupdate, 1, true, [], []};
        _ ->
            Graph = build_graph(NewMinVersion)
    end,
    save_graph(Graph, NewMinVersion),
    remove_old_versions(NewMinVersion),
    update_min_version(NewMinVersion),
    ok.

-spec build_graph(integer()) -> tuple().
build_graph(RequestedMinVersion) ->
    NewMinVersion = min(max(get_min_version(), RequestedMinVersion), get_max_version()),
    GraphUpdates = lists:takewhile(
        fun({graphupdate, VersionNumber, _, _, _}) -> VersionNumber =< NewMinVersion end,
        protobuf_list_to_tuple_list(get_graph_updates(get_min_version()))
    ),
    NewGraph = lists:foldl(
        fun(Update, Graph) -> merge_update_with_graph(Update, Graph) end,
        get_current_full_graph(),
        GraphUpdates
    ),
    {graphupdate, _, _, Added, Deleted} = NewGraph,
    {graphupdate, NewMinVersion, true, Added, Deleted}.

-spec get_current_full_graph() -> tuple().
get_current_full_graph() ->
    protobufs_to_tuple(
        get_graph_updates_for_version(
            get_min_version()
        )
    ).

-spec merge_update_with_graph(tuple(), tuple()) -> tuple().
merge_update_with_graph(Update, Graph) ->
    {_, _, _, ResultingAdditions, _} = Graph,
    {_, _, _, Additions, Deletes} = Update,
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
    {graphupdate, 0, true, TotalDeletionsAdditions, []}.

-spec update_min_version(integer()) -> any().
update_min_version(NewMinVersion) ->
    redis:set("min_version", NewMinVersion).

-spec get_new_min_version() -> integer().
get_new_min_version() ->
    round((get_min_version() + get_max_version()) / 2).

-spec save_graph(tuple(), integer()) -> any().
save_graph(Graph, NewMinVersion) ->
    redis:set("version_" ++ integer_to_list(NewMinVersion), hrp_pb:encode(Graph)).

-spec remove_old_versions(integer()) -> any().
remove_old_versions(NewMinVersion) ->
    lists:map(
        fun(Version) -> redis:remove("version_" ++ integer_to_list(Version)) end,
        lists:seq(get_min_version(), NewMinVersion - 1)
    ).

-spec protobuf_list_to_tuple_list(list()) -> list().
protobuf_list_to_tuple_list(List) ->
    lists:map(
        fun(Item) -> protobufs_to_tuple(Item) end,
        List
    ).

-spec protobufs_to_tuple(list()) -> tuple().
protobufs_to_tuple(Data) ->
    hrp_pb:decode_graphupdate(Data).

-spec add_node(list(), integer(), binary()) -> tuple().
add_node(IPaddress, Port, PublicKey) ->
    NodeId = get_unique_node_id(),
    Hash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
    redis:set("node_hash_" ++ NodeId, Hash),
    Version = get_max_version() + 1,
    set_max_version(Version),
    GraphUpdate = hrp_pb:encode(
            {graphupdate, Version, false, [{node, NodeId, IPaddress, Port, PublicKey, []}], []}
    ),
    redis:set(
        "version_" ++ integer_to_list(Version),
        GraphUpdate
    ),
    UpdateMessage = get_wrapped_graphupdate_message('GRAPHUPDATERESPONSE', GraphUpdate),
    publish(node_update, UpdateMessage),
    {NodeId, Hash}.

-spec get_unique_node_id() -> list().
get_unique_node_id() ->
    NodeId = base64:encode_to_string(crypto:strong_rand_bytes(20)),
    case redis:get("node_hash_" ++ NodeId) of
        undefined ->
            NodeId;
        _ ->
            get_unique_node_id()
    end.


-spec remove_node(list()) -> atom().
remove_node(NodeId) ->
    redis:remove("node_hash_" ++ NodeId),
    Version = get_max_version() + 1,
    set_max_version(Version),
    GraphUpdate =  hrp_pb:encode(
            {graphupdate, Version, false, [], [{node, NodeId, "", 0, "", []}]}
    ),
    redis:set(
        "version_" ++ integer_to_list(Version),
        GraphUpdate
    ),
    UpdateMessage = get_wrapped_graphupdate_message('GRAPHUPDATERESPONSE', GraphUpdate),
    publish(node_update, UpdateMessage),
    ok.

-spec set_max_version(integer()) -> any().
set_max_version(Version) ->
    redis:set("max_version", Version).

-spec get_node_secret_hash(list()) -> list().
get_node_secret_hash(NodeId) ->
    try
        binary_to_list(redis:get("node_hash_" ++ NodeId))
    catch
        _:_  ->
            undefined
    end.

-spec update_node(list(), list(), integer(), binary()) -> atom().
update_node(NodeId, IPaddress, Port, PublicKey) ->
    DeleteVersion = get_max_version() + 1,
    AddVersion = DeleteVersion + 1,
    set_max_version(AddVersion),
    redis:set(
        "version_" ++ integer_to_list(DeleteVersion),
        hrp_pb:encode(
            {graphupdate, DeleteVersion, false, [], [
                {node, NodeId, "", 0, "", []}
            ]}
        )
    ),
    GraphUpdate = hrp_pb:encode(
            {graphupdate, AddVersion, false, [
                {node, NodeId, IPaddress, Port, PublicKey, []}
            ], []}
    ),
    redis:set(
        "version_" ++ integer_to_list(AddVersion),
        GraphUpdate
    ),
    UpdateMessage = get_wrapped_graphupdate_message('GRAPHUPDATERESPONSE', GraphUpdate),
    publish(node_update, UpdateMessage),
    ok.

publish(Event, Data) ->
    gproc:send({p, l, {ws_handler, Event}}, {ws_handler, Event, Data}).

-spec get_wrapped_graphupdate_message(list(), list()) -> list().
get_wrapped_graphupdate_message(Type, Msg) ->
    EncodedMessage  =hrp_pb:encode({graphupdateresponse, [Msg]}),
    hrp_pb:encode({wrapper, Type, EncodedMessage}).
