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
  CurrentVersion = ets:new(graph, [ordered_set, public]),
  AddedNodes = ets:new(addednodes, [ordered_set, public]),
  Node1Edges = ets:new(node1edges, [ordered_set, public]),
  ets:insert(Node1Edges, {"2", 2.0}),
  ets:insert(Node1Edges, {"3", 3.0}),
  ets:insert(Node1Edges, {"4", 4.0}),
  ets:insert(AddedNodes, {"1", "135.346.334.3", "8643", "sfhjrerhre", Node1Edges}),
  ets:insert(CurrentVersion, {1, true, AddedNodes, [],[]}),
  [{table,CurrentVersion} | Config].

end_per_testcase(node_graph_manager_test, Config) ->
  ets:delete(?config(table, Config)).

%%test will fail, no logical test
node_graph_manager_test(Config) ->
   CurrentVersion = ?config(table, Config),
    [{david, 12}] = ets:lookup(CurrentVersion, david).