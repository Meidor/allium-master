%%%-------------------------------------------------------------------
%% @doc graph public API
%% @end
%%%-------------------------------------------------------------------
-module(node_graph_manager).

-export([get_graph_updates/1, rebuild_graph/0, rebuild_graph_at_interval/1, fill_data/0, get_data/0, get_full_graph_data/0, build_graph/1, merge_update_with_graph/2]).

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

fill_data() ->
    redis:set("version_10", hrp_pb:encode(
        {graphupdate, 10, false,
            [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}],
            [],
            []
        }
    )),
    redis:set("version_11", hrp_pb:encode(
        {graphupdate, 11, false,
            [],
            [],
            [{node, "2", "192.168.0.3", 80, "abc123", []}]
        }
    )),
    redis:set("version_12", hrp_pb:encode(
        {graphupdate, 12, false,
            [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}],
            [],
            []
        }
    )).

get_data() ->
    io:format("~p~n~p~n~p~n", [redis:get("version_10"), redis:get("version_11"), redis:get("version_12")]).

get_full_graph_data() ->
    io:format("~p~n", [iolist_to_binary(hrp_pb:encode(
        {graphupdate, 12345, false,
            [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}, {node, "5", "192.168.0.3", 80, "abc123", []}, {node, "5", "192.168.0.3", 80, "abc123", []}],
            [],
            []
        }
    ))]).

