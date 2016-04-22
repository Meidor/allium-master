%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2016 13:36
%%%-------------------------------------------------------------------
-module(graph_SUITE).

%%Commontest lib
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([get_graph_updates_without_existing_graph_test/1, get_graph_updates_test/1, build_graph_test/1, build_graph_without_existing_graph_test/1,
    rebuild_graph_without_existing_graph_test/1, rebuild_graph_test/1, build_graph_test_impossible_actions/1, merge_update_with_graph_test/1, add_node_test/1, remove_node_test/1, get_node_secret_hash_test/1, update_node_test/1]).

all() -> [get_graph_updates_without_existing_graph_test, get_graph_updates_test, build_graph_test, build_graph_without_existing_graph_test,
    rebuild_graph_without_existing_graph_test, rebuild_graph_test, build_graph_test_impossible_actions, merge_update_with_graph_test, add_node_test, remove_node_test, get_node_secret_hash_test, update_node_test].

%%initial for datastructure graph, not right yet
init_per_testcase(_, Config) ->
    meck:new(redis, [non_strict]),
    Config.

end_per_testcase(_, Config) ->
    Config.

get_graph_updates_without_existing_graph_test(_) ->
    %{graphupdate, 1, true, [], [], []})
    VERSION1 = <<8, 1, 16, 1>>,
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "version_1" ->
                VERSION1
        end
    end),

    [VERSION1] = node_graph_manager:get_graph_updates(-1),
    [VERSION1] = node_graph_manager:get_graph_updates(0),
    [] = node_graph_manager:get_graph_updates(1),
    [] = node_graph_manager:get_graph_updates(2).

%%    true = test_helpers:check_function_called(node_graph_manager, get_min_version, []),
%%    true = test_helpers:check_function_called(redis, get, ["min_version"]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_max_version, []),
%%    true = test_helpers:check_function_called(redis, get, ["max_version"]),
%%    false = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [-1]),
%%    false = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [0]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [1]),
%%    false = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [2]),

get_graph_updates_test(_) ->
    %{graphupdate,12345,false,[{node,"2","192.168.0.1",80,"abcdef123456",[{edge,"1",5.0}]}],[],[]}
    VERSION10 = <<8, 185, 96, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34,
        12, 97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,12345,false,[{node,"5","192.168.0.3",80,"abc123",[]}],[],[]}
    VERSION11 = <<8, 185, 96, 16, 0, 26, 26, 10, 1, 53, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34,
        6, 97, 98, 99, 49, 50, 51>>,
    %{graphupdate,12345,false,[{node,"3","192.168.0.5",80,"abcde,fbasfvgassf123456",[{edge,"1",5.0}]}],[],[]}
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
    [] = node_graph_manager:get_graph_updates(12),
    [] = node_graph_manager:get_graph_updates(13).

%%    true = test_helpers:check_function_called(node_graph_manager, get_min_version, []),
%%    true = test_helpers:check_function_called(redis, get, ["min_version"]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_max_version, []),
%%    true = test_helpers:check_function_called(redis, get, ["max_version"]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [11]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [10]),
%%    false = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [-100]),
%%    false = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [9]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_graph_updates_for_versions, [11, 12]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [10, 11, 12]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_version_numbers_since, [12]),

build_graph_without_existing_graph_test(_) ->
    %{graphupdate, 1, true, [], [], []})
    VERSION1 = <<8, 1, 16, 1>>,
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "version_1" ->
                VERSION1
        end
    end),

    {graphupdate, 1, true,
        [],
        [],
        []
    } = node_graph_manager:build_graph(0),
    {graphupdate, 1, true,
        [],
        [],
        []
    } = node_graph_manager:build_graph(1),
    {graphupdate, 1, true,
    [],
    [],
    []
    } = node_graph_manager:build_graph(2).

%%    true = test_helpers:check_function_called(node_graph_manager, get_min_version, []),
%%    true = test_helpers:check_function_called(redis, get, ["min_version"]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_graph_updates, [1]),
%%    true = test_helpers:check_function_called(node_graph_manager, merge_update_with_graph, [1, {graphupdate, 1, true, [], [], []}]),
%%    true = test_helpers:check_function_called(node_graph_manager, get_current_full_graph, []),

build_graph_test(_) ->
    %{graphupdate,10,false,[{node,"2","192.168.0.1",80,"abcdef123456",[{edge,"1",5.000000e+00}]}],[],[]})
    ADD2 = <<8, 10, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34, 12,
        97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,11,false,[{node,"2","192.168.0.1",80,"abcdef123456",[],[],[]})
    REMOVE2 = <<8, 11, 16, 0, 42, 26, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    %{graphupdate,11,false,[{node, "5", "192.168.0.3", 80, "abc123", []}],[],[]}
    ADD5 = <<8, 11, 16, 0, 26, 26, 10, 1, 53, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    %{graphupdate,12,false,[{node, "3", "192.168.0.3", 80, "abc123", []}],[],[]}
    ADD3 = <<8, 12, 16, 0, 26, 26, 10, 1, 51, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    %{graphupdate,12,false,[{node,"2","192.168.0.1",80,"abcdef123456",[{edge,"1",5.000000e+00}]}],[],[]})
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

    {graphupdate, 10, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
        ],
        [],
        []
    } = node_graph_manager:build_graph(9),
    {graphupdate, 10, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
        ],
        [],
        []
    } = node_graph_manager:build_graph(10),
    {graphupdate, 12, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]},
            {node, "5", "192.168.0.3", 80, "abc123", []},
            {node, "3", "192.168.0.3", 80, "abc123", []}
        ],
        [],
        []
    } = node_graph_manager:build_graph(12),
    {graphupdate, 12, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]},
            {node, "5", "192.168.0.3", 80, "abc123", []},
            {node, "3", "192.168.0.3", 80, "abc123", []}
        ],
        [],
        []
    } = node_graph_manager:build_graph(13),

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

build_graph_test_impossible_actions(_) ->
    %{graphupdate,10,false,[{node,"2","192.168.0.1",80,"abcdef123456",[{edge,"1",5.000000e+00}]}],[],[]})
    ADD2 = <<8, 10, 16, 0, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34, 12,
        97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,11,false,[{node,"2","123.0.0.0",80,"abcdef123456",[{edge,"1",5.000000e+00}]}],[],[]})
    ADDDIFFERENT2 = <<8, 11, 16, 0, 26, 40, 10, 1, 50, 18, 9, 49, 50, 51, 46, 48, 46, 48, 46, 48, 24, 80, 34, 12,
        97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,11,false,[{node,"3","192.168.0.1",80,"abcdef123456",[]}],[],[]})
    REMOVE3 = <<8, 11, 16, 0, 42, 26, 10, 1, 51, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34, 6, 97,
        98, 99, 49, 50, 51>>,
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"11">>;
            "version_10" ->
                ADD2;
            "version_11" ->
                ADDDIFFERENT2
        end
    end),

    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"11">>;
            "version_10" ->
                ADD2;
            "version_11" ->
                REMOVE3
        end
    end),
    {graphupdate, 11, true,
        [
            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
        ],
        [],
        []
    } = node_graph_manager:build_graph(11).

rebuild_graph_without_existing_graph_test(_) ->
    %{graphupdate, 1, true, [], [], []})
    REBUILTGRAPH = <<8, 1, 16, 1>>,
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "version_1" ->
                REBUILTGRAPH
        end
    end),
    meck:expect(redis, set, fun(Key, _Value) ->
        case Key of
            _ ->
                ok
        end
    end),
    meck:expect(redis, remove, fun(Key) ->
        case Key of
           _ ->
                ok
        end
    end),
    ok = node_graph_manager:rebuild_graph().

rebuild_graph_test(_) ->
    %{graphupdate,10,true,[{node,"2","192.168.0.1",80,"abcdef123456",[{edge,"1",5.0}]}],[],[]}
    VERSION10 = <<8, 10, 16, 1, 26, 42, 10, 1, 50, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 49, 24, 80, 34,
        12, 97, 98, 99, 100, 101, 102, 49, 50, 51, 52, 53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,11,false,[{node,"5","192.168.0.3",80,"abc123",[]}],[],[]}
    VERSION11 = <<8, 11, 16, 0, 26, 26, 10, 1, 53, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 51, 24, 80, 34,
        6, 97, 98, 99, 49, 50, 51>>,
    %{graphupdate,12,false,[{node,"3","192.168.0.5",80,"abcde,fbasfvgassf123456",[{edge,"1",5.0}]}],[],[]}
    VERSION12 = <<8, 12, 16, 0, 26, 53, 10, 1, 51, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 53, 24, 80, 34,
        23, 97, 98, 99, 100, 101, 44, 102, 98, 97, 115, 102, 118, 103, 97, 115, 115, 102, 49, 50, 51, 52,
        53, 54, 42, 8, 10, 1, 49, 21, 0, 0, 160, 64>>,
    %{graphupdate,13,false,[{node,"4","192.168.0.4",80,"abc123",[]}],[],[]}
    VERSION13 = <<8, 13, 16, 0, 26, 26, 10, 1, 52, 18, 11, 49, 57, 50, 46, 49, 54, 56, 46, 48, 46, 52, 24, 80, 34,
        6, 97, 98, 99, 49, 50, 51>>,
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"13">>;
            "version_10" ->
                VERSION10;
            "version_11" ->
                VERSION11;
            "version_12" ->
                VERSION12;
            "version_13" ->
                VERSION13
        end
    end),
    meck:expect(redis, set, fun(Key, Value) ->
        case Key of
            "min_version" ->
                Value;
            _ ->
                ok
        end
    end),
    meck:expect(redis, remove, fun(Key) ->
        case Key of
            _ ->
                ok
        end
    end),

    ok = node_graph_manager:rebuild_graph().

merge_update_with_graph_test(_) ->
    %The empty graph is merged with an empty update, which results in an empty graph
    {graphupdate, _, true, [], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], []},
            {graphupdate, 11, true, [], [], []}
        ),
    %The empty graph is merged with an update with a new node, which is added to the new graph
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []},
            {graphupdate, 11, true, [], [], []}
        ),
    %The graph with one node is merged with an empty update, which results in the same graph as before the update
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], []},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    %The graph with one node is merged with an update deleting that node, which results in an empty graph
    {graphupdate, _, true, [], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    %The graph with one node is merged with an update deleting a non-exisiting node, which results in the same graph as before the update
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "3", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    %%The graph with one node is merged with an update adding another node, which results in a graph with two nodes
    {graphupdate, _, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}, {node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ),
    %%The graph with two nodes is merged with an update deleting one of them, which results in a graph with only the other node
    {graphupdate, _, true, [{node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}, {node, "5", "192.168.0.5", 80, "abcdef1234567", [{edge, "1", 7.0}]}], [], []}
        ),
    %%The graph with one nodes is merged with an update which adds and deletes the same node, which results in the same graph as before the update
    {graphupdate, _, true, [{node, "5", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []} =
        node_graph_manager:merge_update_with_graph(
            {graphupdate, 12, false, [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
            {graphupdate, 11, true, [{node, "5", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}], [], []}
        ).

add_node_test(_) ->
    NewNodeIP = "123.0.0.1",
    NewNodePort = 1234,
    NewPublicKey = "abcdefghijklmnop",
    NewMaxVersion = "13",
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "min_version" ->
                <<"10">>;
            "max_version" ->
                <<"12">>;

            _ ->
                undefined
        end
    end),

    meck:expect(redis, set, fun(Key, Value) ->
        case Key of
            _ ->
                ok
            end
        end),

    {NodeId, SecretHash} = node_graph_manager:add_node(NewNodeIP, NewNodePort, NewPublicKey),
    true = test_helpers:check_function_called(redis, set, ["max_version", list_to_integer(NewMaxVersion)]),
    true = test_helpers:check_function_called(redis, set, ["node_hash_" ++ NodeId, SecretHash]),
    HashedNewGraphUpdate = (["version_" ++ NewMaxVersion, hrp_pb:encode({graphupdate, list_to_integer(NewMaxVersion), false, [{node, NodeId, NewNodeIP, NewNodePort, NewPublicKey, []}], [], []})]),
    true = test_helpers:check_function_called(redis, set, HashedNewGraphUpdate).

remove_node_test(_) ->
    RemovableNodeId = "YWJjZGVmZ2hpamtsbW4=",
    NewMaxVersion = "13",
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
                {graphupdate, 13, false, [], [], [{node, RemovableNodeId, _, _, _, []}]}
                    = hrp_pb:decode_graphupdate(iolist_to_binary(Value));
            "max_version" ->
                13 = Value
        end
    end),

    meck:expect(redis, remove, fun(Key) ->
        case Key of
            "node_hash_" ++ RemovableNodeId ->
                ok
        end
    end),
    ok = node_graph_manager:remove_node(RemovableNodeId),
    true = test_helpers:check_function_called(redis, set, ["max_version", list_to_integer(NewMaxVersion)]),
    HashedNewGraphUpdate = hrp_pb:encode({graphupdate, list_to_integer(NewMaxVersion), false, [], [], [{node, RemovableNodeId, "", 0, "", []}]}),
    true = test_helpers:check_function_called(redis, set, ["version_" ++ NewMaxVersion, HashedNewGraphUpdate]),
    true = test_helpers:check_function_called(redis, remove, ["node_hash_" ++ RemovableNodeId]).


get_node_secret_hash_test(_) ->
    ExistingNodeId = "YWJjZGVmZ2hpamtsbW4=",
    ExistingNodeHash =  <<"dspjihg8732ftv8ybnsd78vt7324tn">>,
    NonExistingNodeId = "JSDOJFDOJFOKJG34=",
    meck:expect(redis, get, fun(Key) ->
        case Key of
            "node_hash_" ++ ExistingNodeId ->
                ExistingNodeHash;
            _ ->
                undefined
        end
    end),

    ExistingNodeHash = node_graph_manager:get_node_secret_hash(ExistingNodeId),
    undefined = node_graph_manager:get_node_secret_hash(NonExistingNodeId).

update_node_test(_) ->
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
            "node_hash_" ++ _ ->
                ok;
            "version_13" ->
                {graphupdate, 13, false, [], [], [{node, "YWJjZGVmZ2hpamtsbW5vcA==", _, _, _, _}]}
                    = hrp_pb:decode_graphupdate(iolist_to_binary(Value));
            "version_14" ->
                {graphupdate, 14, false, [{node, "YWJjZGVmZ2hpamtsbW5vcA==", "127.0.0.1", 12345, "xyz", []}], [], []}
                    = hrp_pb:decode_graphupdate(iolist_to_binary(Value));
            "max_version" ->
                true = lists:any(
                    fun(X) -> X =:= Value end,
                    [13, 14]
                )
        end
    end),

    ok = node_graph_manager:update_node("YWJjZGVmZ2hpamtsbW5vcA==", "127.0.0.1", 12345, "xyz").
