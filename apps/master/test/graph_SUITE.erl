%%%-------------------------------------------------------------------
%%% @author Koen & Eef
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2016 13:36
%%%-------------------------------------------------------------------
-module(graph_SUITE).
-author("Koen & Eef").

%%Commontest lib
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([node_graph_manager_test/1, build_graph_test/1, merge_update_with_graph_test/1, add_node_test/1, remove_node_test/1, get_node_secret_hash_test/1]).

all() -> [node_graph_manager_test, build_graph_test, merge_update_with_graph_test, add_node_test, remove_node_test, get_node_secret_hash_test].

%%initial for datastructure graph, not right yet
init_per_testcase(_, Config) ->
    meck:new(redis, [non_strict]),
    Config.

end_per_testcase(_, Config) ->
    Config.

node_graph_manager_test(_) ->
    VERSION10 = <<8, 185, 96, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34,
        12, 97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    VERSION11 = <<8, 185, 96, 16, 0, 26, 26, 10, 1, 53, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34,
        6, 97, 98, 99, 49, 50, 51>>,
    VERSION12 = <<8, 185, 96, 16, 0, 26, 53, 10, 1, 51, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 53, 24, 80, 34,
        23, 97, 98, 99, 100, 101, 44, 102, 98, 97, 115, 102, 118, 103, 97, 115, 115, 102, 49, 50, 51, 52,
        53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    meck:expect(redis, get, fun(Key) ->
            case Key of
                "min_version" ->
                    <<"10">>;
                "max_version" ->
                    <<"12">>;
                "version_10" ->
                    VERSION10;
                "version_11" ->
                    VERSION11;
                "version_12" ->
                    VERSION12
            end
        end),

    [VERSION12] = node_graph_manager:get_graph_updates(11),
    [VERSION11, VERSION12] = node_graph_manager:get_graph_updates(10),
    [VERSION10, VERSION11, VERSION12] = node_graph_manager:get_graph_updates(-100),
    [VERSION10, VERSION11, VERSION12] = node_graph_manager:get_graph_updates(9),
    [] = node_graph_manager:get_graph_updates(12).

build_graph_test(_) ->
    ADD2 = <<8, 10, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34, 12,
        97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    REMOVE2 = <<8, 11, 16, 0, 42, 26, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    ADD5 = <<8, 11, 16, 0, 26, 26, 10, 1, 53, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    ADD3 = <<8, 12, 16, 0, 26, 26, 10, 1, 51, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    ADD2VERSION12 = <<8, 12, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34, 12,
        97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,

    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>;
            "version_10" ->
                ADD2;
            "version_11" ->
                ADD5;
            "version_12" ->
                ADD3
        end
    end),

    {graphupdate, 12, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]},
            {node, "5", "192.168.0.3", 80, "abc123", []},
            {node, "3", "192.168.0.3", 80, "abc123", []}],
        [],
        []
    } = node_graph_manager:build_graph(12),
    {graphupdate, 10, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
        ], [], []
    } = node_graph_manager:build_graph(10),

    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>;
            "version_10" ->
                ADD2;
            "version_11" ->
                REMOVE2;
            "version_12" ->
                ADD3
        end
    end),

    {graphupdate, 12, true,
        [
            {node, "3", "192.168.0.3", 80, "abc123", []}],
        [],
        []
    } = node_graph_manager:build_graph(12),
    {graphupdate, 11, true,
        [],
        [],
        []
    } = node_graph_manager:build_graph(11),

    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>;
            "version_10" ->
                ADD2;
            "version_11" ->
                REMOVE2;
            "version_12" ->
                ADD2VERSION12
        end
    end),
    {graphupdate, 12, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
        ],
        [],
        []
    } = node_graph_manager:build_graph(12).

merge_update_with_graph_test(_) ->
    {graphupdate, _, true, [], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], []},
            {graphupdate, 11, true, [], [], []}
        ),
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []},
            {graphupdate, 11, true, [], [], []}
        ),
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], []},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    {graphupdate, _, true, [], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "3", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}, {node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    {graphupdate, _, true, [{node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}, {node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []}
        ),
    {graphupdate, _, true, [{node, "5", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "5", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ).

add_node_test(_) ->
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>;
            "node_hash_YWJjZGVmZ2hpamtsbW4=" ->
                "etsbhdbas";
            "node_hash_YWJjZGVmZ2hpamtsbW5vcA==" ->
                undefined
        end
    end),

    meck:expect(redis, set, fun(Key, Value) ->
        case Key of
            "node_hash_YWJjZGVmZ2hpamtsbW5vcA==" ->
                ok;
            "node_hash_YWJjZGVmZ2hpamtsbW4=" ->
                ok;
            "version_13" ->
                {graphupdate, 13, false, [{node, "YWJjZGVmZ2hpamtsbW5vcA==", "123.0.0.1", 1234, "abcdefghijklmnop", []}], [], []}
                    = hrp_pb:decode_graphupdate(iolist_to_binary(Value));
            "max_version" ->
                13 = Value
        end
    end),

    {"YWJjZGVmZ2hpamtsbW5vcA==", _} = node_graph_manager:add_node("123.0.0.1", 1234, "abcdefghijklmnop").
%%     error = node_graph_manager:add_node("123.0.0.1", 1234, "abcdefghijklmn").

remove_node_test(_) ->
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>
        end
    end),

    meck:expect(redis, set, fun(Key, Value) ->
        case Key of
            "version_13" ->
                {graphupdate, 13, false, [], [], [{node, "YWJjZGVmZ2hpamtsbW4=", _, _, _, []}]}
                    = hrp_pb:decode_graphupdate(iolist_to_binary(Value));
            "max_version" ->
                13 = Value
        end
    end),

    meck:expect(redis, remove, fun(Key) ->
        case Key of
            "node_hash_YWJjZGVmZ2hpamtsbW4=" ->
                ok;
            "node_hash_YWJjZGVmZ2hpamtsbW5vcA==" ->
                "value"
        end
    end),
    ok = node_graph_manager:remove_node("YWJjZGVmZ2hpamtsbW4=").


get_node_secret_hash_test(_) ->
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "node_hash_YWJjZGVmZ2hpamtsbW4=" ->
                <<"dspjihg8732ftv8ybnsd78vt7324tn">>
        end
    end),
    <<"dspjihg8732ftv8ybnsd78vt7324tn">> = node_graph_manager:get_node_secret_hash("YWJjZGVmZ2hpamtsbW4=").