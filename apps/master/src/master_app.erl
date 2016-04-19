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
    {ok, Socket} = gen_tcp:listen(Port, [{active, true}, {packet, raw}]),
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
            io:format("RESPONSE: ~p~n", [Response]),
            gen_tcp:send(Socket, Response);
        _ ->
            unexpected
    end.

handle_message(Msg) ->
    DecodedMsg = hrp_pb:delimited_decode_encryptedwrapper(iolist_to_binary(Msg)),
    {[{encryptedwrapper, Type, Key, Data} | _], _} = DecodedMsg,
    case Type of
        'GRAPHUPDATEREQUEST' ->
            Request = hrp_pb:decode_graphupdaterequest(Data),
            get_wrapped_message(
                'GRAPHUPDATERESPONSE',
                hrp_pb:encode(
                    {graphupdateresponse, 12345, false,
                        [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}],
                        [{node, "1", "192.168.0.2", 80, "zyx123", [{edge, "2", 2.0}]}],
                        [{node, "3", "192.168.0.3", 80, "abc123",[]}]
                    }
                )
            );
        'NODEREGISTERREQUEST' ->
            {noderegisterrequest, IPaddress, Port, PublicKey} = hrp_pb:decode_noderegisterrequest(Data),
            {NodeId, SecretHash} = node_service:node_register(IPaddress, Port, PublicKey),
            get_wrapped_message(
                'NODEREGISTERRESPONSE',
                hrp_pb:encode(
                    {noderegisterresponse, 'SUCCES', NodeId, SecretHash}
                )
            );
        'NODEUPDATEREQUEST' ->
            {NodeId, SecretHash, IPaddress, Port, PublicKey} = hrp_pb:decode_nodeupdaterequest(Data),
            node_service:node_update(NodeId, SecretHash, IPaddress, Port, PublicKey),
            get_wrapped_message(
                'NODEUPDATERESPONSE',
                hrp_pb:encode(
                    {nodeupdateresponse, 'SUCCES'}
                )
            );
        'NODEDELETEREQUEST' ->
            {NodeId, SecretHash} = hrp_pb:decode_nodedeleterequest(Data),
            node_service:node_unregister(NodeId, SecretHash),
            get_wrapped_message(
                'NODEDELETERESPONSE',
                hrp_pb:encode(
                    {nodedeleteresponse, 'SUCCES'}
                )
            );
        'CLIENTREQUEST' ->
            Request = hrp_pb:decode_clientrequest(Data),
            get_wrapped_message(
                'CLIENTRESPONSE',
                hrp_pb:encode(
                    {clientresponse,[{client, "123abc", "123456abcde", [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]},
                        {client, "456def", "654321fedcba", [{node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}]}]}
                )
            );
        'CLIENTHEARTBEAT' ->
            Request = hrp_pb:decode_clientheartbeat(Data),
            get_wrapped_message(
                'CLIENTRESPONSE',
                hrp_pb:encode(
                    {clientresponse,[{"USER", "KEY123", []}]}
                )
            );
        'NODEHEARTBEAT' ->
            Request = hrp_pb:decode_nodeheartbeat(Data),
            get_wrapped_message(
                'CLIENTRESPONSE',
                hrp_pb:encode(
                    {clientresponse,[{"USER", "KEY123", []}]}
                )
            );
        'CLIENTREGISTERREQUEST' ->
            Request = hrp_pb:decode_clientregisterrequest(Data),
            get_wrapped_message(
                'CLIENTREGISTERRESPONSE',
                hrp_pb:encode(
                    {clientregisterresponse, 'SUCCES'}
                )
            );
        'CLIENTLOGINREQUEST' ->
            Request = hrp_pb:decode_clientloginrequest(Data),
            get_wrapped_message(
                'CLIENTLOGINRESPONSE',
                hrp_pb:encode(
                    {clientloginresponse, 'SUCCES', "KEY123", []}
                )
            );
        'CLIENTLOGOUTREQUEST' ->
            Request = hrp_pb:decode_clientlogoutrequest(Data),
            get_wrapped_message(
                'CLIENTLOGOUTRESPONSE',
                hrp_pb:encode(
                    {clientlogoutresponse, 'SUCCES'}
                )
            )
    end.

get_wrapped_message(Type, Msg) ->
    hrp_pb:encode({encryptedwrapper, Type, "123456789", Msg}).
