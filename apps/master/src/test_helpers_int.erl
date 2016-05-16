-module(test_helpers_int).

-include_lib("common_test/include/ct.hrl").

-export([
    get_data_encrypted_response/3,
    encode_message_to_binary/1,
    send_heartbeat/2,
    register_node_return_id/2,
    pass_to_next_test/1,
    retrieve_from_last_test/2,
    valid_secret_hash/1,
    valid_id/1,
    empty_database/0,
    get_connection/0
]).

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

-spec send_heartbeat(binary(), atom()) -> any().
send_heartbeat(Message, RequestType) ->
    master_app:handle_message(hrp_pb:encode([{wrapper, RequestType, Message}])).

-spec register_node_return_id(list(), binary()) -> list().
register_node_return_id(IP, PublicKey) ->
    RegisterRequest = {noderegisterrequest, IP, 80, PublicKey},
    {noderegisterresponse, 'SUCCES', Id, _SecretHash} = hrp_pb:decode_noderegisterresponse(
        get_data_encrypted_response(RegisterRequest, 'NODEREGISTERREQUEST', 'NODEREGISTERRESPONSE')),
    Id.

-spec pass_to_next_test(list()) -> tuple().
pass_to_next_test(List) ->
    {save_config, List}.

-spec retrieve_from_last_test(atom(), any()) -> any().
retrieve_from_last_test(Info, Config)->
    {_, OldConfig} = ?config(saved_config, Config),
    ?config(Info, OldConfig).

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