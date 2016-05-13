%%%===================================================================
%% @doc master public API
%% @end
%%%===================================================================

-module(master_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1,
    start/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(any(), any()) -> any().
start(_StartType, _StartArgs) ->
    Link = master_sup:start_link(),
    start(1337),
    lager:info("Start listening on port 1337..."),
    persistence_service:init(),
    lager:info("Mnesia started..."),
    timer:sleep(14400000),
    Link.

-spec stop(any()) -> atom().
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start(integer()) -> any().
start(Port) ->
    spawn(fun() -> server(Port) end).

-spec server(integer()) -> any().
server(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{active, true}, {packet, raw}]),
    listen(Socket).

-spec listen(any()) -> any().
listen(Socket) ->
    {ok, Active_socket} = gen_tcp:accept(Socket),
    Handler = spawn(fun() -> handle_messages(Active_socket) end),
    ok = gen_tcp:controlling_process(Active_socket, Handler),
    listen(Socket).

-spec handle_messages(any()) -> any().
handle_messages(Socket) ->
    receive
        {tcp, error, closed} ->
            done;
        {tcp, Socket, Data} ->
            lager:info("~p", [Data]),
            Response = handle_message(Data),
            lager:info("RESPONSE: ~p", [Response]),
            gen_tcp:send(Socket, Response),
            handle_messages(Socket);
        _ ->
            unexpected
    end.

-spec handle_message(list()) -> list().
handle_message(Msg) ->
    DecodedMsg = hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    lager:info("MSG: ~p DECODED: ~p", [Msg, DecodedMsg]),
    {[{wrapper, Type, Data} | _], _} = DecodedMsg,
    case Type of
        'GRAPHUPDATEREQUEST' ->
            {graphupdaterequest, Version} = hrp_pb:decode_graphupdaterequest(Data),
            get_wrapped_message(
                'GRAPHUPDATERESPONSE',
                hrp_pb:encode(
                    {graphupdateresponse, node_graph_manager:get_graph_updates(Version)}
                )
            );
        'NODEREGISTERREQUEST' ->
            {noderegisterrequest, IPaddress, Port, PublicKey}
                = hrp_pb:decode_noderegisterrequest(Data),
            try
                node_service:node_register(IPaddress, Port, PublicKey)
            of {NodeId, SecretHash} ->
                get_wrapped_message(
                    'NODEREGISTERRESPONSE',
                    hrp_pb:encode(
                        {noderegisterresponse, 'SUCCES', NodeId, SecretHash}
                    )
                )
            catch
                error:alreadyexists ->
                    get_wrapped_message(
                        'NODEREGISTERRESPONSE',
                        hrp_pb:encode(
                            {noderegisterresponse, 'ALREADY_EXISTS', undefined, undefined}
                        )
                    );
                _:Error ->
                    lager:error("Error in node register request: ~p", [Error]),
                    get_wrapped_message(
                        'NODEREGISTERRESPONSE',
                        hrp_pb:encode(
                            {noderegisterresponse, 'FAILED', undefined, undefined}
                        )
                    )
            end;
        'NODEUPDATEREQUEST' ->
            {nodeupdaterequest, NodeId, SecretHash, IPaddress, Port, PublicKey}
                = hrp_pb:decode_nodeupdaterequest(Data),
            try
                node_service:node_update(NodeId, SecretHash, IPaddress, Port, PublicKey)
            of _ ->
                get_wrapped_message(
                    'NODEUPDATERESPONSE',
                    hrp_pb:encode(
                        {nodeupdateresponse, 'SUCCES'}
                    )
                )
            catch _:Error ->
                lager:error("Error in node update request: ~p", [Error]),
                get_wrapped_message(
                    'NODEUPDATERESPONSE',
                    hrp_pb:encode(
                        {nodeupdateresponse, 'FAILED'}
                    )
                )
            end;
        'NODEDELETEREQUEST' ->
            {nodedeleterequest, NodeId, SecretHash} = hrp_pb:decode_nodedeleterequest(Data),
            try
                node_service:node_unregister(NodeId, SecretHash)
            of _ ->
                get_wrapped_message(
                    'NODEDELETERESPONSE',
                    hrp_pb:encode(
                        {nodedeleteresponse, 'SUCCES'}
                    )
                )
            catch _:Error ->
                lager:error("Error in node delete request: ~p", [Error]),
                get_wrapped_message(
                    'NODEDELETERESPONSE',
                    hrp_pb:encode(
                        {nodedeleteresponse, 'FAILED'}
                    )
                )
            end;
        'CLIENTREQUEST' ->
            Request = hrp_pb:decode_clientrequest(Data),
            get_wrapped_message(
                'CLIENTRESPONSE',
                hrp_pb:encode(
                    {clientresponse,[
                        {client, "123abc", "123456abcde", [
                            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
                        ]},
                        {client, "456def", "654321fedcba", [
                            {node, "2", "192.168.0.1", 80, "abcdef123456", [{edge, "1", 5.0}]}
                        ]}
                    ]}
                )
            );
        'CLIENTHEARTBEAT' ->
            {clientheartbeat, Username, SecretHash} = hrp_pb:decode_clientheartbeat(Data),
            heartbeat_monitor:receive_heartbeat_client(Username, SecretHash);
        'NODEHEARTBEAT' ->
            {nodeheartbeat, Id, SecretHash} = hrp_pb:decode_nodeheartbeat(Data),
            heartbeat_monitor:receive_heartbeat_node(Id, SecretHash);
        'CLIENTREGISTERREQUEST' ->
            {clientregisterrequest, Username, Password} = hrp_pb:decode_clientregisterrequest(Data),
            try client_service:client_register(Username, Password) of
            ok ->
                get_wrapped_message(
                    'CLIENTREGISTERRESPONSE',
                    hrp_pb:encode(
                        {clientregisterresponse, 'SUCCES'}
                    )
                )
            catch
                error:usernametaken ->
                    get_wrapped_message(
                        'CLIENTREGISTERRESPONSE',
                        hrp_pb:encode(
                            {clientregisterresponse, 'TAKEN_USERNAME'}
                        )
                    );
                _:Error ->
                    lager:error("Error in client register request: ~p", [Error]),
                    get_wrapped_message(
                        'CLIENTREGISTERRESPONSE',
                        hrp_pb:encode(
                            {clientregisterresponse, 'FAILED'}
                        )
                    )
            end;
        'CLIENTLOGINREQUEST' ->
            {clientloginrequest, Username, Password, PublicKey} = hrp_pb:decode_clientloginrequest(Data),
            try
                client_service:client_login(Username, Password, PublicKey)
            of {SecretHash, ConnectedNodes} ->
                get_wrapped_message(
                    'CLIENTLOGINRESPONSE',
                    hrp_pb:encode(
                        {clientloginresponse, 'SUCCES', SecretHash, ConnectedNodes}
                    )
                )
            catch
                error:invalidclient ->
                    get_wrapped_message(
                        'CLIENTLOGINRESPONSE',
                        hrp_pb:encode(
                            {noderegisterresponse, 'INVALID_COMBINATION', undefined, undefined}
                        )
                    );
                _:Error ->
                    lager:error("Error in client login request: ~p", [Error]),
                    get_wrapped_message(
                        'NODEREGISTERRESPONSE',
                        hrp_pb:encode(
                            {noderegisterresponse, 'FAILED', undefined, undefined}
                        )
                    )
            end;
        'CLIENTLOGOUTREQUEST' ->
            {clientlogoutrequest, Username, SecretHash} = hrp_pb:decode_clientlogoutrequest(Data),
            try
                client_service:client_logout(Username, SecretHash) of
            ok ->
                get_wrapped_message(
                    'CLIENTLOGOUTRESPONSE',
                    hrp_pb:encode(
                        {clientlogoutresponse, 'SUCCES'}
                    )
                )
            catch
                error:clientnotverified ->
                    get_wrapped_message(
                        'CLIENTLOGOUTRESPONSE',
                        hrp_pb:encode(
                            {clientlogoutresponse, 'UNVERIFIEDUSER'}
                        )
                    )
            end
    end.

-spec get_wrapped_message(list(), list()) -> list().
get_wrapped_message(Type, Msg) ->
    hrp_pb:encode([{wrapper, Type, Msg}]).
