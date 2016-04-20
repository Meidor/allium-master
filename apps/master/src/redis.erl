%%%-------------------------------------------------------------------
%%% @author Raoul
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2016 10:25
%%%-------------------------------------------------------------------
-module(redis).
-author("Raoul").

%% API
-export([get/1, set/2, remove/1, get_matching_keys/1, get_list/1]).

get(Key) ->
    {ok, Connection} = eredis:start_link(),
    {ok, Value} = eredis:q(Connection, ["GET", "onion_" ++ Key]),
    Value.

get_matching_keys(Key) ->
    {ok, Connection} = eredis:start_link(),
    {ok, Keys} = eredis:q(Connection, ["KEYS", "onion_" ++ Key ++ "*"]),
    Keys.

get_list([])->
    [];
get_list(ListOfKeys) ->
    {ok, Connection} = eredis:start_link(),
    {ok, ListOfKeyValues} = eredis:q(Connection, ["MGET" | ListOfKeys]),
    ListOfKeyValues.

set(Key, Value) ->
    {ok, Connection} = eredis:start_link(),
    eredis:q(Connection, ["SET", "onion_" ++ Key, Value]).

remove(Key) ->
    {ok, Connection} = eredis:start_link(),
    eredis:q(Connection, ["DEL", "onion_" ++ Key]).