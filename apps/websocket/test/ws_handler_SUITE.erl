%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2016 13:36
%%%-------------------------------------------------------------------
-module(ws_handler_SUITE).

%%Commontest lib
-include_lib("common_test/include/ct.hrl").

%% API
-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).
-export([]).

all() -> [].

%%initial for datastructure graph, not right yet
init_per_testcase(_, Config) ->
    meck:new(gproc),
    meck:expect(gproc, send, fun(_,_) -> ok end),
    meck:expect(gproc, reg, fun(_,_) -> ok end),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(gproc),
    Config.