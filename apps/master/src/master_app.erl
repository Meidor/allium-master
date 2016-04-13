%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/1]).
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
  start(1337),
  timer:sleep(1000000),
  Binary = hrp_pb:encode(
    [
      {message, "abc123", "Dit is de tekst", "Raoul"},
      {message, "def", "Dit is de tekst!!!", "Niels"}
    ]),
  ?debugFmt("~n~p~n", [iolist_to_binary(Binary)]),
  ?assert(true).

start(Port) ->
  spawn(fun() -> server(Port) end).

server(Port) ->
  {ok, Socket} = gen_tcp:listen(Port,[{active, true}, {packet, raw}]),
  listen(Socket).

listen(Socket) ->
  {ok, Active_socket} = gen_tcp:accept(Socket),
  Handler = spawn(fun() -> handle_messages(Active_socket) end),
  ok = gen_tcp:controlling_process(Active_socket, Handler),
  listen(Socket).

handle_messages(Socket) ->
  receive
    {tcp, error, closed} ->
      done;
    {tcp, Socket, Data} ->
      io:format("~p~n", [Data]),
      Response = handle_message(Data),
      gen_tcp:send(Socket, Response);
    _ ->
      unexpected
  end.

handle_message(Msg) ->
  Data = hrp_pb:delimited_decode_message(iolist_to_binary(Msg)),
  io:format("~p~n", [Data]),
  hrp_pb:encode({message, "12345", "werkt kei goe", "Raoul"}).
%%   MessageType = element(1, hd(Data)),
%%   case MessageType of
%%     message ->
%%       hrp_pb:encode({message, "id", "text", "username"});
%%     _ ->
%%       ok
%%   end.
