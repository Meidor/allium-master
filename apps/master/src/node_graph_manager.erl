%%%-------------------------------------------------------------------
%% @doc graph public API
%% @end
%%%-------------------------------------------------------------------
-module(node_graph_manager).

-export([get_graph_updates/1]).

get_graph_updates(Version) ->
    get_graph_updates_for_versions(
        get_version_numbers_since(
            max(Version, get_min_version())
        )
    ).

get_version_numbers_since(Version) ->
    lists:seq(Version + 1, get_max_version()).

get_min_version() ->
    %% Redis get
    10.

get_max_version() ->
    %% Redis get
    12.

get_graph_updates_for_versions(Versions) ->
    lists:map(
        fun(Version) -> get_graph_updates_for_version(Version) end,
        Versions
    ).

get_graph_updates_for_version(Version) ->
    %% Redis get
    {Version, false, [], [], []}.
