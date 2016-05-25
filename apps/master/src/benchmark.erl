-module(benchmark).

%% API
-export([
    benchmark/0, benchmark/1
]).

benchmark() ->
    benchmark(1000).

benchmark(Count) ->
    test_helpers_int:empty_database(),
    persistence_service:delete_all_clients(),
    benchmark_node_register(Count),
    benchmark_graph_updates_small(Count),
    benchmark_graph_updates_large(Count),
    benchmark_graph_updates_all(round(Count / 10)),
    benchmark_client_register(Count),
    benchmark_client_register_and_login(Count),
    benchmark_client_list(Count).


benchmark_node_register(Count) ->
    Message = get_wrapped_message(
        'NODEREGISTERREQUEST',
        hrp_pb:encode(
            {noderegisterrequest, "123.123.123.123", 123, <<"OK">>}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Registered ~p nodes in ~p ms on average per node.~n", [Count, Time / Count]).

benchmark_graph_updates_small(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, Count - 10}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched 10 graph updates ~p times in ~p ms on average per request.~n", [Count, Time / Count]).

benchmark_graph_updates_large(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, Count - 100}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched 100 graph updates ~p times in ~p ms on average per request.~n", [Count, Time / Count]).

benchmark_graph_updates_all(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, 0}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched ~p graph updates ~p times in ~p ms on average per request.~n", [Count * 10, Count, Time / Count]).

benchmark_client_register(Count) ->
    Message = fun() ->
        get_wrapped_message(
            'CLIENTREGISTERREQUEST',
            hrp_pb:encode(
                {clientregisterrequest, "user" ++ integer_to_list(erlang:unique_integer()), "test123"}
            )
        )
    end,
    Time = benchmark_message(Message, Count),
    lager:info("Registered ~p users in ~p ms on average per request.~n", [Count, Time / Count]).

benchmark_client_register_and_login(Count) ->
    Message = fun() ->
        Integer = erlang:unique_integer(),
        get_wrapped_message(
            'CLIENTREGISTERREQUEST',
            hrp_pb:encode(
                {clientregisterrequest, "user" ++ integer_to_list(Integer), "test123"}
            )
        ) ++
        get_wrapped_message(
            'CLIENTLOGINREQUEST',
            hrp_pb:encode(
                {clientloginrequest, "user" ++ integer_to_list(Integer), "test123", <<"key">>}
            )
        )
    end,
    Time = benchmark_message(Message, Count),
    lager:info("Registered and logged in ~p users in ~p ms on average per request.~n", [Count, Time / Count]).

benchmark_client_list(Count) ->
    Message = get_wrapped_message(
        'CLIENTREQUEST',
        hrp_pb:encode(
            {clientrequest, 1}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched ~p clients ~p times in ~p ms on average per request.~n", [Count, Count, Time / Count]).



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
send_message_multiple_times(MessageFun, Count) when is_function(MessageFun) ->
    Self = self(),
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect("127.0.0.1", 1337, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
        lists:map(
            fun(Message) ->
                gen_tcp:send(Socket, Message),
                gen_tcp:recv(Socket, 0)
            end,
            MessageFun()
        ),
        gen_tcp:close(Socket),
        Self ! Count - 1
    end);
send_message_multiple_times(Message, Count) ->
    Self = self(),
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect("127.0.0.1", 1337, [{packet, 0}, {active, false}, {reuseaddr, true}]),
        gen_tcp:send(Socket, Message),
        gen_tcp:recv(Socket, 0),
        gen_tcp:close(Socket),
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
