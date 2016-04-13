%%%-------------------------------------------------------------------
%% @doc master public API
%% @end
%%%-------------------------------------------------------------------

-module(master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
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
  start_server(31337),
  timer:sleep(1000000),
  Binary = hrp_pb:encode(
    [
      {message, "abc123", "Dit is de tekst", "Raoul"},
      {message, "def", "Dit is de tekst!!!", "Niels"}
    ]),
  ?debugFmt("~n~p~n", [iolist_to_binary(Binary)]),
  ?assert(true).

start_server(Port) ->
  Pid = spawn_link(fun() ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn(fun() -> acceptor(Listen) end),
    timer:sleep(infinity)
                   end),
  {ok, Pid}.

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      Response = handle_message(Msg),
      gen_tcp:send(Socket, Response),
      handle(Socket)
  end.

handle_message(Msg) ->
  Data = hrp_pb:delimited_decode_message(list_to_binary(Msg)),
  MessageType = element(1, hd(Data)),
  case MessageType of
    message ->
      hrp_pb:encode({message, "id", "text", "username"});
    _ ->
      ok
  end.
