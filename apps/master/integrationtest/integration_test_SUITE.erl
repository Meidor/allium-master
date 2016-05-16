-module(integration_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_suite/1
]).

-export([
    node_integration_test/1,
    client_registration_test/1
]).

all() -> [
    node_integration_test,
    client_registration_test
].

init_per_suite(Config) ->
    empty_database(),
    persistence_service:init(),
    persistence_service:delete_all_clients(),
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    empty_database(),
    persistence_service:delete_all_clients(),
    Config.

node_integration_test(_Config) ->
    % A non-existing node is updated, will return error
    NonExistingUpdateRequest =
        {nodeupdaterequest, "12345", "secrethash12345", "255.255.255.255", 80, <<"generatedpublickey">>},
    {nodeupdateresponse, 'FAILED'} = hrp_pb:decode_nodeupdateresponse(
        get_data_encrypted_response(NonExistingUpdateRequest, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')),

    erlang:display("non_existing node could not be updated"),

    % A non-existing node is deleted, will return error
    NonExistingDeleteRequest = {nodedeleterequest, "12345", "secrethash12345"},
    {nodedeleteresponse, 'FAILED'} = hrp_pb:decode_nodedeleteresponse(
        get_data_encrypted_response(NonExistingDeleteRequest, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')),

    erlang:display("non_existing node could not be deleted"),

    % A non-existing node sends a heartbeat, will not be added to redis
    NonExistingHeartbeat = encode_message_to_binary({nodeheartbeat, "12345", "secrethash12345"}),
    try
        send_heartbeat(NonExistingHeartbeat, 'NODEHEARTBEAT', 0),
        error(shouldnotbereached)
    catch
        error:nodenotverified ->
            undefined = redis:get("onion_heartbeat_node_12345");
        _:_ ->
            error(testdidnotpass)
    end,

    erlang:display("non_existing node could not be added to heartbeat monitoring"),

    % A node is registered.
    RegisterRequest = {noderegisterrequest, "255.255.255.255", 80, <<"generatedpublickey">>},
    {noderegisterresponse, 'SUCCES', Id, SecretHash} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    true = valid_id(Id),
    true = valid_secret_hash(SecretHash),

    erlang:display("node is registered"),

    % The node sends a heartbeat and it is verified the node is included in the database
    Heartbeat = encode_message_to_binary({nodeheartbeat, Id, SecretHash}),
    send_heartbeat(Heartbeat, 'NODEHEARTBEAT', 0),
    true = is_binary(redis:get("heartbeat_node_" ++ Id)),

    erlang:display("node has sent its heartbeat"),

    %%TODO At this point this returns SUCCES
    % The node is registered again which will now result in an 'already exists' error
    {noderegisterresponse, 'ALREADY_EXISTS', "", ""} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),

    erlang:display("node could not be registered again"),

    % Verify the node is added to the graph
    GraphUpdateRequestOne = {graphupdaterequest, 0},
    {graphupdateresponse, GraphOneUpdates} = hrp_pb:decode_graphupdateresponse(
        get_data_encrypted_response(GraphUpdateRequestOne, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphOneUpdates = [hrp_pb:decode_graphupdate(GraphOneUpdate) || GraphOneUpdate <- GraphOneUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, Id, "255.255.255.255", 80,  <<"generatedpublickey">>, []}], []}
    ] = DecodedGraphOneUpdates,

    erlang:display("The registered node is in the graph updates"),

    % Sending a heartbeat again
    send_heartbeat(Heartbeat, 'NODEHEARTBEAT', 0),

    % The node is updated
    UpdateRequest = {nodeupdaterequest, Id, SecretHash, "255.255.255.100", 50, <<"newlygeneratedpublickey">>},
    {nodeupdateresponse, 'SUCCES'} = hrp_pb:decode_nodeupdateresponse(
        get_data_encrypted_response(UpdateRequest, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE')),

    erlang:display("The registered node is updated"),

    % Sending a heartbeat again
    send_heartbeat(Heartbeat, 'NODEHEARTBEAT', 0),

    % Verify the node is added to the graph
    GraphUpdateRequestTwo = {graphupdaterequest, 0},
    {graphupdateresponse, GraphTwoUpdates} = hrp_pb:decode_graphupdateresponse(
        get_data_encrypted_response(GraphUpdateRequestTwo,'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphTwoUpdates = [hrp_pb:decode_graphupdate(GraphTwoUpdate) || GraphTwoUpdate <- GraphTwoUpdates],
    [
        {graphupdate, 1, true, [], []},
        {graphupdate, 2, false, [{node, Id, "255.255.255.255", 80, <<"generatedpublickey">>, []}], []},
        {graphupdate, 3, false, [], [{node, Id, [] , 0, <<>>, [] }]},
        {graphupdate, 4, false, [{node, Id, "255.255.255.100", 50, <<"newlygeneratedpublickey">>, []}], []}
    ] = DecodedGraphTwoUpdates,

    erlang:display("The update of the registered node is in the graph updates"),

    % A node is deleted, will return error
    DeleteRequest = {nodedeleterequest, Id, SecretHash},
    {nodedeleteresponse, 'SUCCES'} = hrp_pb:decode_nodedeleteresponse(
        get_data_encrypted_response(DeleteRequest, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE')),

    erlang:display("The registered node is deleted"),

    % Verify the node is deleted to the graph
    GraphUpdateRequestThree = {graphupdaterequest, 3},
    {graphupdateresponse, GraphThreeUpdates} = hrp_pb:decode_graphupdateresponse(
        get_data_encrypted_response(GraphUpdateRequestThree, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphThreeUpdates = [hrp_pb:decode_graphupdate(GraphThreeUpdate) || GraphThreeUpdate <- GraphThreeUpdates],
    [
        {graphupdate, 4, false, [{node, Id, "255.255.255.100", 50, <<"newlygeneratedpublickey">>, []}], []},
        {graphupdate, 5, false,[], [{node, Id, [], 0, <<>> , []}]}
    ] = DecodedGraphThreeUpdates,
    undefined = redis:get("heartbeat_node_" ++ Id),

    erlang:display("The deletion of the registered node is in the graph updates"),

    % Wait for graph to be automatically rebuild
    %test_server:sleep(33000),
    node_graph_manager:rebuild_graph(),

    % Verify the graph is rebuild
    GraphUpdateRequestFour = {graphupdaterequest, 0},
    {graphupdateresponse, GraphFourUpdates} = hrp_pb:decode_graphupdateresponse(
        get_data_encrypted_response(GraphUpdateRequestFour, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphFourUpdates = [hrp_pb:decode_graphupdate(GraphFourUpdate) || GraphFourUpdate <- GraphFourUpdates],
    [
        {graphupdate, 3, true, [], []},
        {graphupdate, 4, false, [{node, Id , "255.255.255.100", 50, <<"newlygeneratedpublickey">>, []}], []},
        {graphupdate, 5, false,[], [{node, Id, [], 0, <<>>, []}]}
    ] = DecodedGraphFourUpdates,

    erlang:display("The graph has automatically rebuild"),

    %%Add two nodes and wait until one is automatically removed because it does not send a heartbeat
    {noderegisterresponse, 'SUCCES', IdTwo, SecretHashTwo} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),

    RegisterOtherNodeRequest =
        {noderegisterrequest, "255.255.0.118", 80, <<"generatedpublickey">>},
    {noderegisterresponse, 'SUCCES', IdThree, _SecretHashThree} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterOtherNodeRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),

    HeartbeatTwo = encode_message_to_binary({nodeheartbeat, IdTwo, SecretHashTwo}),

    % Wait 4 intervals of eight seconds and send heartbeat for one of the nodes
    IntervalsToSleep = [1000, 1000, 1000, 1000, 1000],
    [send_heartbeat(HeartbeatTwo, 'NODEHEARTBEAT', Interval) || Interval <- IntervalsToSleep],

    heartbeat_monitor:remove_inactive_nodes(3),
    node_graph_manager:rebuild_graph(),

    % Verify the node is removed
    GraphUpdateRequestFive = {graphupdaterequest, 0},
    {graphupdateresponse, GraphFiveUpdates} = hrp_pb:decode_graphupdateresponse(
        get_data_encrypted_response(GraphUpdateRequestFive, 'GRAPHUPDATEREQUEST', 'GRAPHUPDATERESPONSE')),
    DecodedGraphFiveUpdates = [hrp_pb:decode_graphupdate(GraphFiveUpdate) || GraphFiveUpdate <- GraphFiveUpdates],
    [
        {graphupdate, 6, true, [{node, IdTwo, "255.255.255.255", 80, <<"generatedpublickey">>, []}], []},
        {graphupdate, 7, false, [{node, IdThree, "255.255.0.118", 80, <<"generatedpublickey">>, []}], []},
        {graphupdate, 8, false, [], [{node, IdThree, [], 0, <<>>, []}]}
    ] = DecodedGraphFiveUpdates,

    erlang:display("The node was removed after not sending a heartbeat for too long").

client_registration_test(_Config) ->
    % Log in with a non-existing client and return an error
    NonExistingLoginRequest = {clientloginrequest, "username", "Password1234", <<"generatedpublickey">>},
    {clientloginresponse, 'INVALID_COMBINATION', "", []} = hrp_pb:decode_clientloginresponse(
        get_data_encrypted_response(NonExistingLoginRequest, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),

    erlang:display("A non-existing client could not be logged in"),

    % Register a client
    RegisterRequest = {clientregisterrequest, "username", "Password1234"},
    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        get_data_encrypted_response(RegisterRequest, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')),

    erlang:display("A client is registered"),

    % Log in with the registered client
    LoginRequest = {clientloginrequest, "username", "Password1234", <<"generatedpublickey">>},
    {clientloginresponse, 'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
        get_data_encrypted_response(LoginRequest, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),
    true = valid_secret_hash(SecretHash),
    [] = DedicatedNodes,

    erlang:display("The registered client could be logged in"),

    % Request all clients, return the one registered client
    ClientRequest = {clientrequest, 1},
    {clientresponse, [
        {client, "username", <<"generatedpublickey">>, []}
    ]} = hrp_pb:decode_clientresponse(get_data_encrypted_response(ClientRequest, 'CLIENTREQUEST', 'CLIENTRESPONSE')),

    erlang:display("The clients are requested"),

    % Refrain from sending a heartbeat so the client is removed
    timer:sleep(2000),
    heartbeat_monitor:remove_inactive_clients(1),

    % Verify the secret hash of the user is undefined after not sending a heartbeat for too long
    {"username", undefined, <<"generatedpublickey">>, "Password1234", []} =
        persistence_service:select_client("username"),

    erlang:display("After not sending a heartbeat, the client secret hash is now undefined"),

    % Register a client with the same username as existing client, this won't work
    RegisterSameUsernameRequest = {clientregisterrequest, "username", "Password697"},
    {clientregisterresponse, 'TAKEN_USERNAME'} = hrp_pb:decode_clientregisterresponse(
        get_data_encrypted_response(RegisterSameUsernameRequest, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')),

    erlang:display("A client with an already existing username could not be registered"),

    % Register another client
    RegisterNewClientRequest = {clientregisterrequest, "anotherusername", "Password1234"},
    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        get_data_encrypted_response(RegisterNewClientRequest, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE')),

    erlang:display("Another client is registered"),

    % Three nodes are registered to serve as connected nodes
    NodeIds = [register_node_return_id(IP, Key) || {IP, Key} <- [{"255.255.0.1", <<"publickeyone">>},
        {"255.255.0.2", <<"publickeytwo">>}, {"255.255.0.3", <<"publickeythree">>}]],

    %%TODO DedicatedNodes are sent as nodes according to protocol, should be NodeIds,
    % Log in new client
    OtherLoginRequest = {clientloginrequest, "anotherusername", "Password1234", <<"anothergeneratedpublickey">>},
    {clientloginresponse, 'SUCCES', OtherSecretHash, OtherDedicatedNodes} = hrp_pb:decode_clientloginresponse(
        get_data_encrypted_response(OtherLoginRequest, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE')),

    % It is verified that there are three nodes attached to the client at login and those nodes are the nodes
    % that were created earlier
    3 = length(lists:filter(fun(NodeId) -> lists:member(NodeId, OtherDedicatedNodes) end, NodeIds)),

    erlang:display("After registering nodes, the new client has nodes after logging in"),

    % Log out the new client to verify that the secrethash will be set to undefined
    LogoutRequest = {clientlogoutrequest, "anotherusername", OtherSecretHash},
    {clientlogoutresponse, 'SUCCES'} = hrp_pb:decode_clientlogoutresponse(
        get_data_encrypted_response(LogoutRequest, 'CLIENTLOGOUTREQUEST', 'CLIENTLOGOUTRESPONSE')),

    {"anotherusername", undefined, <<"anothergeneratedpublickey">>, "Password1234", []} =
        persistence_service:select_client("anotherusername"),

    erlang:display("The new client is logged out and his secret hash is now undefined").

-spec get_data_encrypted_response(binary(), atom(), atom()) -> tuple().
get_data_encrypted_response(Message, RequestType, ResponseType) ->
    {[{wrapper, RetrievedResponseType, Data} | _], _} = hrp_pb:delimited_decode_wrapper(
        iolist_to_binary(master_app:handle_message(
            hrp_pb:encode(
                [{wrapper, RequestType, encode_message_to_binary(Message)}]
            )))),
    RetrievedResponseType = ResponseType,
    Data.

-spec encode_message_to_binary(tuple()) -> binary().
encode_message_to_binary(Message) ->
    iolist_to_binary(
        hrp_pb:encode(
            Message
        )).

-spec send_heartbeat(binary(), atom(), integer()) -> any().
send_heartbeat(Message, RequestType, Interval) ->
    timer:sleep(Interval),
    master_app:handle_message(hrp_pb:encode([{wrapper, RequestType, Message}])).

-spec register_node_return_id(list(), binary()) -> list().
register_node_return_id(IP, PublicKey) ->
    RegisterRequest = {noderegisterrequest, IP, 80, PublicKey},
    {noderegisterresponse, 'SUCCES', Id, _SecretHash} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    Id.

-spec valid_secret_hash(list()) -> any().
valid_secret_hash(SecretHash) ->
    true = is_list(SecretHash) and (length(SecretHash) > 10).

-spec valid_id(list()) -> any().
valid_id(Id) ->
    true = is_list(Id) and (length(Id) > 0).

-spec empty_database() -> any().
empty_database() ->
    eredis:q(get_connection(), ["FLUSHALL"]).

-spec get_connection() -> pid().
get_connection() ->
    case whereis(redis) of
        undefined ->
            {ok, Connection} = eredis:start_link(),
            register(redis, Connection),
            Connection;
        Pid ->
            Pid
    end.
