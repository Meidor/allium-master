-module(ws_client_handler).

-behaviour(websocket_client).

-export([
    start_link/0,
    init/1,
    onconnect/2,
    ondisconnect/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3,
    send/3,
    recv/1
]).

-spec start_link() -> any().
start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("ws://localhost:8080/websocket", ?MODULE, []).

-spec init(list()) -> tuple().
init([]) ->
    {once, state}.

-spec onconnect(any(), any()) -> tuple().
onconnect(_WSReq, State) ->
    {ok, State}.

-spec ondisconnect(tuple(), any()) -> tuple().
ondisconnect({remote, closed}, State) ->
    {ok, State}.

-spec send(pid(), atom(), binary()) -> any().
send(Pid, Type, Msg) ->
    Message = hrp_pb:encode([{wrapper, Type, Msg}]),
    websocket_client:cast(Pid, {binary, iolist_to_binary(Message)}).

-spec recv(pid()) -> any().
recv(Pid) ->
    Pid ! {binary, self()},
    receive
        Message ->
            Message
    after
        4000 ->
            noresponse
    end.

-spec websocket_handle(any(), any(), any()) -> any().
websocket_handle({text, _Msg}, _Req, State) ->
    {ok, State};
websocket_handle({binary, Msg}, _Req, State) ->
    {Type, Data} = get_message_from_wrapper(Msg),
    handle_response(Type, Data, State);
websocket_handle(_Data, _Req, State) ->
    {ok, State}.

-spec handle_response(atom(), binary(), any()) -> any().
handle_response('ADMINLOGINRESPONSE', Data, State) ->
    State ! hrp_pb:decode_adminloginresponse(Data),
    {ok, State};
handle_response('GRAPHUPDATERESPONSE', Data, State) ->
    State ! hrp_pb:decode_graphupdateresponse(Data),
    {ok, State};
handle_response('ADMINLISTRESPONSE', Data, State) ->
    State ! hrp_pb:decode_adminlistresponse(Data),
    {ok, State}.

-spec get_message_from_wrapper(iolist()) -> tuple().
get_message_from_wrapper(Msg) ->
    MessageWrapper =  hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    {[{wrapper, Type, Data} | _], _} = MessageWrapper,
    {Type, Data}.

-spec websocket_info(tuple(), any(), any()) -> tuple().
websocket_info({binary, From}, _Req, _State) ->
    {ok, From};
websocket_info(Data, _Req, _State) ->
    self() ! Data,
    {ok, Data}.

-spec websocket_terminate(any(), any(), any()) -> atom().
websocket_terminate(_Reason, _ConnState, _State) ->
    ok.