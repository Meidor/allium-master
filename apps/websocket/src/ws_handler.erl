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
websocket_handle({text, _Msg}, Req, State) ->
    {ok, Req, State};
    websocket_handle({binary, Msg}, Req, loggged_in) ->
    DecodedMsg = hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    {[{wrapper, Type, Data} | _], _} = DecodedMsg,
    case Type of
        'ADMINREGISTERREQUEST' ->
            {adminregisterrequest, Username, Password, Superadmin} = hrp_pb:decode_adminregisterrequest(Data),
            try persistence_service:insert_admin(Username, Password, Superadmin) of
                ok ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, persistence_service:select_all_admins()}
                        )
                    )
            catch
                error:usernametaken ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    );
                _:Error ->
                    lager:error("Error in admin register request: ~p", [Error]),
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    )
            end;
        'ADMINUPDATEREQUEST' ->
            {adminupdaterequest, Username, Password, Superadmin} = hrp_pb:decode_adminupdaterequest(Data),
            try persistence_service:update_admin(Username, Password, Superadmin) of
                ok ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, persistence_service:select_all_admins()}
                        )
                    )
            catch
                error:usernametaken ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    );
                _:Error ->
                    lager:error("Error in admin update request: ~p", [Error]),
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    )
            end;
        'ADMINDELETEREQUEST' ->
            {admindeleterequest, Username} = hrp_pb:decode_admindeleterequest(Data),
            try persistence_service:delete_admin(Username) of
                ok ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, persistence_service:select_all_admins()}
                        )
                    )
            catch
                error:usernametaken ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    );
                _:Error ->
                    lager:error("Error in admin delete request: ~p", [Error]),
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    )
            end;
        'ADMINLISTREQUEST' ->
            {adminlistrequest} = hrp_pb:decode_adminlistrequest(Data),
            try persistence_service:select_all_admins() of
                ok ->
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, persistence_service:select_all_admins()}
                        )
                    )
            catch
                _:Error ->
                    lager:error("Error in admin list request: ~p", [Error]),
                    get_wrapped_message(
                        'ADMINLISTRESPONSE',
                        hrp_pb:encode(
                            {adminlistresponse, []}
                        )
                    )
            end
    end;

websocket_handle({binary, Msg}, Req, State) ->
    DecodedMsg = hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    {[{wrapper, Type, Data} | _], _} = DecodedMsg,
    case Type of
        'ADMINLOGINREQUEST' ->
            {adminloginrequest, Username, Password} = hrp_pb:decode_adminloginrequest(Data),
            try auth_service:admin_login(Username, Password) of
                _ ->
                    {reply, {binary, get_wrapped_message('ADMINLOGINRESPONSE',
                        hrp_pb:encode({adminloginresponse, 'SUCCES'}))}, Req, logged_in}
            catch
                _:_ ->
                    {reply, {binary, get_wrapped_message('ADMINLOGINRESPONSE',
                        hrp_pb:encode({adminloginresponse, 'FAILED'}))}, Req, State}
            end
    end;

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
    hrp_pb:encode({wrapper, Type, Msg}).

-spec get_full_graph() -> list().
get_full_graph() ->
    get_wrapped_message(
        'GRAPHUPDATERESPONSE',
        hrp_pb:encode(
            {graphupdateresponse, node_graph_manager:get_graph_updates(0)}
        )
    ).