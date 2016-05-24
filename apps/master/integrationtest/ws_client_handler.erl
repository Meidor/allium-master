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
    send/2,
    recv/1
]).

start_link() ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link("ws://localhost:8080/websocket", ?MODULE, []).

init([]) ->
    {once, state}.

onconnect(_WSReq, State) ->
    {ok, State}.

ondisconnect({remote, closed}, State) ->
    {ok, State}.

send(Pid, Msg) ->
    websocket_client:cast(Pid, {binary, iolist_to_binary(Msg)}).

recv(Pid) ->
    Pid ! {binary, self()},
    receive
        Message ->
            Message
    after
        5000 ->
            noresponse
    end.

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

websocket_info({binary, From}, _Req, _State) ->
    {ok, From};
websocket_info(Data, _Req, _State) ->
    self() ! Data,
    {ok, Data}.

websocket_terminate(_Reason, _ConnState, _State) ->
    erlang:display("Websocket closed in state ~p wih reason ~p~n"),
    ok.