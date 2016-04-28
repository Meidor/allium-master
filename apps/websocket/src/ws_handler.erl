-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

subscribe(Event) ->
    gproc:reg({p, l, {?MODULE, Event}}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    subscribe(node_update),
    {ok, Req, undefined_state, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {binary, Msg}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({?MODULE, node_update, Msg}, Req, State) ->
    {reply, {binary, Msg}, Req, State, hibernate};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.