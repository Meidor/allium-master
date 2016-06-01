%%%===================================================================
%% @doc master public API
%% @end
%%%===================================================================

-module(master_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    start/1,
    stop/1,
    handle_message/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc
%% Start the master application. Makes it listen on port 1337 for requests.
-spec start(any(), any()) -> any().
start(_StartType, _StartArgs) ->
    lager:start(),
    Link = master_sup:start_link(),
    start(1337),
    lager:info("Start listening on port 1337..."),
    persistence_service:init(),
    lager:info("Mnesia started..."),
    redis:init(),
    lager:info("Redis cluster initialised..."),
    timer:sleep(14400000),
    Link.

%% @doc
%% Stops the master application.
-spec stop(any()) -> atom().
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc
%% Starts a process listening on port 1337.
-spec start(integer()) -> any().
start(Port) ->
    spawn(fun() -> server(Port) end).

-spec server(integer()) -> any().
server(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{active, true}, {packet, raw}, {reuseaddr, true}]),
    listen(Socket).

-spec listen(any()) -> any().
listen(Socket) ->
    {ok, Active_socket} = gen_tcp:accept(Socket),
    Handler = spawn(fun() -> handle_messages(Active_socket) end),
    ok = gen_tcp:controlling_process(Active_socket, Handler),
    listen(Socket).

%% @doc
%% Takes requests, checks the type of the requests and sends it to the appropriate function to handle the request.
-spec handle_messages(any()) -> any().
handle_messages(Socket) ->
    receive
        {tcp, error, closed} ->
            done;
        {tcp, Socket, Data} ->
            Response = handle_message(Data),
            gen_tcp:send(Socket, Response),
            handle_messages(Socket);
        _ ->
            unexpected
    after
        300000 ->
            gen_tcp:close(Socket)
    end.

-spec handle_message(list()) -> list().
handle_message(Msg) ->
    DecodedMsg = hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    {[{wrapper, Type, Data} | _], _} = DecodedMsg,
    handle_decoded_message(Type, Data).

-spec handle_decoded_message(atom(), list()) -> list().
handle_decoded_message('GRAPHUPDATEREQUEST', Data) ->
    {graphupdaterequest, Version} = hrp_pb:decode_graphupdaterequest(Data),
    get_wrapped_message(
        'GRAPHUPDATERESPONSE', {graphupdateresponse, node_graph_manager:get_graph_updates(Version)}
    );
handle_decoded_message('NODEREGISTERREQUEST', Data) ->
    {noderegisterrequest, IPaddress, Port, PublicKey} = hrp_pb:decode_noderegisterrequest(Data),
    try
        node_service:node_register(IPaddress, Port, PublicKey)
    of {NodeId, SecretHash} ->
        get_wrapped_message(
            'NODEREGISTERRESPONSE', {noderegisterresponse, 'SUCCES', NodeId, SecretHash}
        )
    catch
        error:alreadyexists ->
            get_wrapped_message(
                'NODEREGISTERRESPONSE',
                {noderegisterresponse, 'ALREADY_EXISTS', undefined, undefined}
            );
        _:Error ->
            lager:error("Error in node register request: ~p", [Error]),
            get_wrapped_message(
                'NODEREGISTERRESPONSE', {noderegisterresponse, 'FAILED', undefined, undefined}
            )
    end;
handle_decoded_message('NODEUPDATEREQUEST', Data) ->
    {nodeupdaterequest, NodeId, SecretHash, IPaddress, Port, PublicKey} =
        hrp_pb:decode_nodeupdaterequest(Data),
    try
        node_service:node_update(NodeId, SecretHash, IPaddress, Port, PublicKey)
    of NodeId ->
        get_wrapped_message(
            'NODEUPDATERESPONSE', {nodeupdateresponse, 'SUCCES', NodeId}
        );
        NewNodeId ->
            get_wrapped_message(
                'NODEUPDATERESPONSE', {nodeupdateresponse, 'SUCCES', NewNodeId}
            )
    catch _:Error ->
        lager:error("Error in node update request: ~p", [Error]),
        get_wrapped_message(
            'NODEUPDATERESPONSE', {nodeupdateresponse, 'FAILED', undefined}
        )
    end;
handle_decoded_message('NODEDELETEREQUEST', Data) ->
    {nodedeleterequest, NodeId, SecretHash} = hrp_pb:decode_nodedeleterequest(Data),
    try
        node_service:node_unregister(NodeId, SecretHash)
    of _ ->
        get_wrapped_message(
            'NODEDELETERESPONSE', {nodedeleteresponse, 'SUCCES'}
        )
    catch _:Error ->
        lager:error("Error in node delete request: ~p", [Error]),
        get_wrapped_message(
            'NODEDELETERESPONSE', {nodedeleteresponse, 'FAILED'}
        )
    end;
handle_decoded_message('CLIENTREQUEST', Data) ->
    {clientrequest, ClientGroup} = hrp_pb:decode_clientrequest(Data),
    get_wrapped_message(
        'CLIENTRESPONSE',
        {clientresponse, client_manager:return_all_clients_by_clientgroup(ClientGroup)}
    );
handle_decoded_message('CLIENTHEARTBEAT', Data) ->
    {clientheartbeat, Username, SecretHash} = hrp_pb:decode_clientheartbeat(Data),
    heartbeat_monitor:receive_heartbeat_client(Username, SecretHash);
handle_decoded_message('NODEHEARTBEAT', Data) ->
    {nodeheartbeat, Id, SecretHash} = hrp_pb:decode_nodeheartbeat(Data),
    heartbeat_monitor:receive_heartbeat_node(Id, SecretHash);
handle_decoded_message('CLIENTREGISTERREQUEST', Data) ->
    {clientregisterrequest, Username, Password} = hrp_pb:decode_clientregisterrequest(Data),
    try client_service:client_register(Username, Password) of
    ok ->
        get_wrapped_message(
            'CLIENTREGISTERRESPONSE', {clientregisterresponse, 'SUCCES'}
        )
    catch
        error:usernametaken ->
            get_wrapped_message(
                'CLIENTREGISTERRESPONSE', {clientregisterresponse, 'TAKEN_USERNAME'}
            );
        _:Error ->
            lager:error("Error in client register request: ~p", [Error]),
            get_wrapped_message(
                'CLIENTREGISTERRESPONSE', {clientregisterresponse, 'FAILED'}
            )
    end;
handle_decoded_message('CLIENTLOGINREQUEST', Data) ->
    {clientloginrequest, Username, Password, PublicKey} = hrp_pb:decode_clientloginrequest(Data),
    try
        client_service:client_login(Username, Password, PublicKey)
    of {SecretHash, ConnectedNodes} ->
        get_wrapped_message(
            'CLIENTLOGINRESPONSE', {clientloginresponse, 'SUCCES', SecretHash, ConnectedNodes}
        )
    catch
        error:clientcredentialsnotvalid ->
            get_wrapped_message(
                'CLIENTLOGINRESPONSE', {clientloginresponse, 'INVALID_COMBINATION', undefined, []}
            );
        _:Error ->
            lager:error("Error in client login request: ~p", [Error]),
            get_wrapped_message(
                'CLIENTLOGINRESPONSE', {clientloginresponse, 'FAILED', undefined, []}
            )
    end;
handle_decoded_message('CLIENTLOGOUTREQUEST', Data) ->
    {clientlogoutrequest, Username, SecretHash} = hrp_pb:decode_clientlogoutrequest(Data),
    try
        client_service:client_logout(Username, SecretHash) of
    ok ->
        get_wrapped_message(
            'CLIENTLOGOUTRESPONSE', {clientlogoutresponse, 'SUCCES'}
        )
    catch
        _:Error ->
            lager:error("Error in client logout request: ~p", [Error]),
            get_wrapped_message(
                'CLIENTLOGOUTRESPONSE',
                {clientlogoutresponse, 'FAILED'}
            )
    end.

-spec get_wrapped_message(list(), list()) -> list().
get_wrapped_message(Type, Msg) ->
    Message = hrp_pb:encode(Msg),
    hrp_pb:encode([{wrapper, Type, Message}]).
