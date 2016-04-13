%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, test_test/0]).
-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    master_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

test_test() ->
  Var = hrp_pb:decode_message(iolist_to_binary(hrp_pb:encode({message, "abc123", "Dit is de tekst", "Raoul"}))),
  ?debugFmt("Function fun1 starting...", [Var]),
  ?assert(true).
