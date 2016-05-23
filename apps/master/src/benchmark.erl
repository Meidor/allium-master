-module(benchmark).

%% API
-export([
    benchmark/1
]).

benchmark(Count) ->
    benchmark_node_register(Count).
%%     benchmark_graph().


benchmark_node_register(Count) ->
    Message = get_wrapped_message(
        'NODEREGISTERREQUEST',
        hrp_pb:encode(
            {noderegisterrequest, "123.123.123.123", 123, <<"OK">>}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Registered ~p nodes in ~p ms on average per node.~n", [Count, Time / Count]).

benchmark_graph() ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, 0}
        )
    ).

benchmark_message(Message, Count) ->
    send_message_multiple_times(Message, 1),
    timer:sleep(1000),
    Start = get_timestamp(),
%%     send_message_multiple_times(Message, Count),
    wait_for_messages_received(Message, Count),
    End = get_timestamp(),
    End - Start.


get_timestamp() ->
    {_, Time, Milli} = erlang:now(),
    Time * 1000 + Milli / 1000.

-spec get_wrapped_message(list(), list()) -> list().
get_wrapped_message(Type, Msg) ->
    hrp_pb:encode([{wrapper, Type, Msg}]).

send_message_multiple_times(_, 0) ->
    ok;
send_message_multiple_times(Message, Count)->
    Self = self(),
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect("127.0.0.1", 1337, [{packet, 0}, {active, false}, {reuseaddr, true}]),
        lists:map(
            fun(_) -> gen_tcp:send(Socket, Message) end,
            lists:seq(1, 10)
        ),
        gen_tcp:shutdown(Socket, read_write),
        Self ! Count - 1
    end).

wait_for_messages_received(_, 0) ->
    ok;
wait_for_messages_received(Message, Count) ->
    receive
        _ ->
            send_message_multiple_times(Message, Count - 1)
    end,
    wait_for_messages_received(Message, Count - 1).
