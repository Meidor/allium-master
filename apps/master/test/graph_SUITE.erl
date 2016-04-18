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
-export([node_graph_manager_test/1]).

all() -> [node_graph_manager_test].

%%initial for datastructure graph, not right yet
init_per_testcase(node_graph_manager_test, Config) ->
    Config.

end_per_testcase(node_graph_manager_test, Config) ->
    Config.

%%test will fail, no logical test
node_graph_manager_test(Config) ->
    [{12, false, [], [], []}] = node_graph_manager:get_graph_updates(11),
    [{11, false, [], [], []}, {12, false, [], [], []}] = node_graph_manager:get_graph_updates(10),
    [{11, false, [], [], []}, {12, false, [], [], []}] = node_graph_manager:get_graph_updates(-100).