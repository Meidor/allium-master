-module(benchmark).

%% API
-export([
    benchmark/0, benchmark/1
]).

-spec benchmark() -> atom().
%% @doc
%% Starts a benchmark with a standard amount of 1000 requests to test.
benchmark() ->
    benchmark(1000).

-spec benchmark(integer()) -> atom().
%% @doc
%% Starts a benchmark, testing how long it takes to register nodes, users and how long
%% it takes to fetch graph updates.
%% These results will then be written to the console.
%% params
%% Count: the amount of entities in the benchmark.
benchmark(Count) ->
    test_helpers_int:empty_database(),
    persistence_service:delete_all_clients(),
    benchmark_node_register(Count),
    benchmark_graph_updates_none(Count),
    benchmark_graph_updates_small(Count),
    benchmark_graph_updates_large(Count),
    benchmark_graph_updates_all(round(Count / 10)),
    benchmark_client_register(Count),
    benchmark_client_register_and_login(Count),
    benchmark_client_list(Count).

-spec benchmark_node_register(integer()) -> atom().
benchmark_node_register(Count) ->
    Message = fun() ->
        get_wrapped_message(
            'NODEREGISTERREQUEST',
            hrp_pb:encode(
                {noderegisterrequest, generate_random_ip(), 123, <<"OK">>}
            )
        )
    end,
    Time = benchmark_message(Message, Count),
    lager:info("Registered ~p nodes in ~p ms on average per node.~n", [Count, Time / Count]).

-spec benchmark_graph_updates_none(integer()) -> atom().
benchmark_graph_updates_none(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, Count}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched 0 graph updates ~p times in ~p ms on average per request.~n",
        [Count, Time / Count]).

-spec benchmark_graph_updates_small(integer()) -> atom().
benchmark_graph_updates_small(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, Count - 10}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched 10 graph updates ~p times in ~p ms on average per request.~n",
        [Count, Time / Count]).

-spec benchmark_graph_updates_large(integer()) -> atom().
benchmark_graph_updates_large(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, Count - 100}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched 100 graph updates ~p times in ~p ms on average per request.~n",
        [Count, Time / Count]).

-spec benchmark_graph_updates_all(integer()) -> atom().
benchmark_graph_updates_all(Count) ->
    Message = get_wrapped_message(
        'GRAPHUPDATEREQUEST',
        hrp_pb:encode(
            {graphupdaterequest, 0}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched ~p graph updates ~p times in ~p ms on average per request.~n",
        [Count * 10, Count, Time / Count]).

-spec benchmark_client_register(integer()) -> atom().
benchmark_client_register(Count) ->
    Message = fun() ->
        get_wrapped_message(
            'CLIENTREGISTERREQUEST',
            hrp_pb:encode(
                {clientregisterrequest, "user" ++ integer_to_list(erlang:unique_integer()),
                    "test123"}
            )
        )
    end,
    Time = benchmark_message(Message, Count),
    lager:info("Registered ~p users in ~p ms on average per request.~n", [Count, Time / Count]).

-spec benchmark_client_register_and_login(integer()) -> atom().
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
    lager:info("Registered and logged in ~p users in ~p ms on average per request.~n",
        [Count, Time / Count]).

-spec benchmark_client_list(integer()) -> atom().
benchmark_client_list(Count) ->
    Message = get_wrapped_message(
        'CLIENTREQUEST',
        hrp_pb:encode(
            {clientrequest, 1}
        )
    ),
    Time = benchmark_message(Message, Count),
    lager:info("Fetched ~p clients ~p times in ~p ms on average per request.~n",
        [Count, Count, Time / Count]).

-spec benchmark_message(list() | fun(), integer()) -> number().
benchmark_message(Message, Count) ->
    send_message(Message, 1),
    timer:sleep(1000),
    Start = get_timestamp(),
    wait_for_messages_received(Message, Count),
    End = get_timestamp(),
    End - Start.

-spec get_timestamp() -> number().
get_timestamp() ->
    {_, Time, Milli} = erlang:timestamp(),
    Time * 1000 + Milli / 1000.

-spec get_wrapped_message(atom(), list()) -> list().
get_wrapped_message(Type, Msg) ->
    hrp_pb:encode([{wrapper, Type, Msg}]).

-spec send_message(list() | fun(), integer()) -> atom().
send_message(_, 0) ->
    ok;
send_message(MessageFun, Count) when is_function(MessageFun) ->
    Self = self(),
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect("127.0.0.1", 1337,
            [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
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
send_message(Message, Count) ->
    Self = self(),
    spawn(fun() ->
        {ok, Socket} = gen_tcp:connect("127.0.0.1", 1337,
            [{packet, 0}, {active, false}, {reuseaddr, true}]),
        gen_tcp:send(Socket, Message),
        gen_tcp:recv(Socket, 0),
        gen_tcp:close(Socket),
        Self ! Count - 1
    end).

-spec wait_for_messages_received(list() | fun(), integer()) -> atom().
wait_for_messages_received(_, 0) ->
    ok;
wait_for_messages_received(Message, Count) ->
    receive
        _ ->
            send_message(Message, Count - 1)
    end,
    wait_for_messages_received(Message, Count - 1).

-spec generate_random_ip() -> list().
generate_random_ip() ->
    generate_random_ip_part() ++ "." ++
    generate_random_ip_part() ++ "." ++
    generate_random_ip_part() ++ "." ++
    generate_random_ip_part().

-spec generate_random_ip_part() -> list().
generate_random_ip_part() ->
    integer_to_list(abs(erlang:unique_integer()) rem 255).
