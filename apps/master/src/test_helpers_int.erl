-module(test_helpers_int).

-include_lib("common_test/include/ct.hrl").

%% API
-export([
    get_data_encrypted_response/3,
    encode_message_to_binary/1,
    send_heartbeat/2,
    register_node_return_id/2,
    register_node/3,
    update_node/5,
    delete_node/2,
    valid_secret_hash/1,
    valid_id/1,
    empty_database/0,
    get_connection/0,
    register_client/2,
    login_client/3,
    init_sharded_eredis/0
]).

%% This module is used to help the integration tests.
%% A lot of tests require certain situations and these function are here to get to those situations.


-spec get_data_encrypted_response(binary(), atom(), atom()) -> tuple().
%% @doc
%% Used to get responses from master_app.
get_data_encrypted_response(Message, RequestType, ResponseType) ->
    {[{wrapper, RetrievedResponseType, Data} | _], _} = hrp_pb:delimited_decode_wrapper(
        iolist_to_binary(
            master_app:handle_message(
                hrp_pb:encode(
                    [{wrapper, RequestType, encode_message_to_binary(Message)}]
                )
            )
        )
    ),
    RetrievedResponseType = ResponseType,
    Data.

-spec encode_message_to_binary(tuple()) -> binary().
%% @doc
%% Used to encode messages.
encode_message_to_binary(Message) ->
    iolist_to_binary(
        hrp_pb:encode(
            Message
        )
    ).

-spec send_heartbeat(binary(), atom()) -> any().
%% @doc
%% Sends a heartbeat to the master_app.
%% This is used to help test functionality that requires a senT heartbeat.
send_heartbeat(Message, RequestType) ->
    master_app:handle_message(hrp_pb:encode([{wrapper, RequestType, Message}])).

-spec register_node_return_id(list(), binary()) -> list().
%% @doc
%% sends a register node request to the master_app.
%% this is used to help test functionality that requires a registered node.
%% this one returns the node Id so that you can use it in your test.
register_node_return_id(IP, PublicKey) ->
    {NodeId, _, _, _, _} = register_node(IP, 80, PublicKey),
    NodeId.

-spec register_node(list(), integer(), binary()) -> any().
%% @doc
%% Sends a node register request to the master_app.
%% This is used to help test functionality that requires a loged in client.
register_node(IP, Port, PublicKey) ->
    Request = {noderegisterrequest, IP, Port, PublicKey},

    {noderegisterresponse, 'SUCCES', NodeId, SecretHash} = hrp_pb:decode_noderegisterresponse(
        test_helpers_int:get_data_encrypted_response(
            Request, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE'
        )
    ),
    {NodeId, SecretHash, IP, Port, PublicKey}.

-spec register_client(list(), list()) -> tuple().
%% @doc
%% Sends a client register request to the master_app.
%% This is used to help test functionality that requires a registered in client.
register_client(Username, Password) ->
    Request = {clientregisterrequest, Username, Password},
    {clientregisterresponse, 'SUCCES'} = hrp_pb:decode_clientregisterresponse(
        test_helpers_int:get_data_encrypted_response(
            Request, 'CLIENTREGISTERREQUEST', 'CLIENTREGISTERRESPONSE'
        )
    ).

-spec login_client(list(), list(), binary()) -> tuple().
%% @doc
%% Sends a client login request to the master_app.
%% This is used to help test functionality that requires a loged in client.
login_client(Username, Password, PublicKey) ->
    Request = {clientloginrequest, Username, Password, PublicKey},
    {clientloginresponse,
        'SUCCES', SecretHash, DedicatedNodes} = hrp_pb:decode_clientloginresponse(
            test_helpers_int:get_data_encrypted_response(
                Request, 'CLIENTLOGINREQUEST', 'CLIENTLOGINRESPONSE'
            )
        ),
    {Username, Password, PublicKey, SecretHash, DedicatedNodes}.

-spec update_node(list(), list(), list(), integer(), binary()) -> any().
%% @doc
%% Sends a update node request to the master_app.
%% This is used to help test functionality that requires a updated node.
update_node(NodeId, SecretHash, IP, Port, PublicKey) ->
    Request = {nodeupdaterequest, NodeId, SecretHash, IP, Port, PublicKey},
    ReturnedNodeId = IP ++ ":" ++ integer_to_list(Port),

    {nodeupdateresponse, 'SUCCES', ReturnedNodeId} = hrp_pb:decode_nodeupdateresponse(
        test_helpers_int:get_data_encrypted_response(
            Request, 'NODEUPDATEREQUEST', 'NODEUPDATERESPONSE'
        )
    ),
    {ReturnedNodeId, SecretHash, IP, Port, PublicKey}.

-spec delete_node(list(), list()) -> any().
%% @doc
%% Sends a delete node request to the master_app.
%% This is used to help test functionality that requires the deletion of a node.
delete_node(NodeId, SecretHash) ->
    Request = {nodedeleterequest, NodeId, SecretHash},
    {nodedeleteresponse, 'SUCCES'} = hrp_pb:decode_nodedeleteresponse(
        test_helpers_int:get_data_encrypted_response(
            Request, 'NODEDELETEREQUEST', 'NODEDELETERESPONSE'
        )
    ).

-spec valid_secret_hash(list()) -> any().
%% @doc
%% Checks if the SecretHash meets the requirements.
valid_secret_hash(SecretHash) ->
    true = is_list(SecretHash) and (length(SecretHash) == 68).

-spec valid_id(list()) -> any().
%% @doc
%% Checks if the Id meets the requirements.
valid_id(Id) ->
    true = is_list(Id).

-spec empty_database() -> any().
%% @doc
%% Used to reset the redis database, not the mnesia database.
empty_database() ->
    redis:apply_to_execute_command_on_all_nodes(["FLUSHALL"], fun(_) -> ok end).

-spec get_connection() -> pid().
%% @doc
%% Used to get a connection to redis.
get_connection() ->
    case whereis(redis) of
        undefined ->
            {ok, Connection} = eredis:start_link(),
            register(redis, Connection),
            Connection;
        Pid ->
            Pid
    end.

-spec init_sharded_eredis() -> any().
%% @doc
%% Used to initialize sharded eredis.
init_sharded_eredis() ->
    application:set_env(sharded_eredis, pools,
        [
            {pool0, [
                {size, 10},
                {max_overflow, 20},
                {host, "127.0.0.1"},
                {port, 6379}
            ]}
        ]
    ),
    application:set_env(sharded_eredis, global_or_local, local),
    sharded_eredis:start().