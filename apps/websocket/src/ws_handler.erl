-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-spec subscribe(any()) -> any().
subscribe(Event) ->
    gproc:reg({p, l, {?MODULE, Event}}),
    RandomEvent = atom_to_list(Event) ++ "_init",
    gproc:reg({p, l, {?MODULE, RandomEvent}}),
    gproc:send({p, l, {?MODULE, RandomEvent}}, {?MODULE, RandomEvent, get_full_graph()}),
    gproc:unreg({p, l, {?MODULE, RandomEvent}}).

-spec init(tuple(), any(), any()) -> tuple().
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

-spec websocket_init(any(), any(), any()) -> tuple().
websocket_init(_TransportName, Req, _Opts) ->
    lager:info("New websocket connection intitialised."),
    subscribe(node_update),
    {ok, Req, undefined_state, hibernate}.

-spec websocket_handle(tuple(), any(), any()) -> tuple().
websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_handle({binary, Msg}, _Req, _State) ->
    lager:info("Received binary message"),
    hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg));
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec websocket_info(tuple(), any(), any()) -> tuple().
websocket_info({?MODULE, _, Msg}, Req, State) ->
    lager:info("Received message from pub/sub sending it through the websocket."),
    {reply, {binary, Msg}, Req, State, hibernate}.

-spec websocket_terminate(any(), any(), any()) -> atom().
websocket_terminate(_Reason, _Req, _State) ->
    ok.

-spec get_wrapped_message(list(), list()) -> list().
 get_wrapped_message(Type, Msg) ->
    hrp_pb:encode([{wrapper, Type, Msg}]).

-spec get_full_graph() -> list().
get_full_graph() ->
    get_wrapped_message(
        'GRAPHUPDATERESPONSE',
        hrp_pb:encode(
            {graphupdateresponse, node_graph_manager:get_graph_updates(0)}
        )
    ).
