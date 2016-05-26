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
websocket_handle({binary, Msg}, Req, State) ->
    {Type, Data} = get_message_from_wrapper(Msg),
    handle_request(Type, Data, Req, State);
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec handle_request(atom(), binary(), any(), any()) -> any().
handle_request('ADMINLOGINREQUEST', Data, Req, State) ->
    {adminloginrequest, Username, Password} = hrp_pb:decode_adminloginrequest(Data),
    try auth_service:admin_login(Username, Password) of
        IsSuperAdmin ->
            {reply, {binary, get_wrapped_message('ADMINLOGINRESPONSE',
                hrp_pb:encode({adminloginresponse, 'SUCCES', IsSuperAdmin}))}, Req, {logged_in, Username}}
    catch
        _:_ ->
            {reply, {binary, get_wrapped_message('ADMINLOGINRESPONSE',
                hrp_pb:encode({adminloginresponse, 'FAILED', false}))}, Req, State}
    end;
handle_request('UPDATENODE', Data, Req, {logged_in, LoggedInUsername}) ->
    {updatenode, Node} = hrp_pb:decode_updatenode(Data),
    {node, Id, IPaddress, Port, PublicKey, Edges} = Node,
    try
        node_service:node_exists(Id),
        Id = IPaddress ++ ":" ++ integer_to_list(Port),
        node_graph_manager:update_node(Id, IPaddress, Port, PublicKey, Edges)
    catch
        _:_ ->
            lager:info(LoggedInUsername ++ " tried to alter information which should not be altered!")
    end,
    {ok, Req, logged_in};
handle_request('ADMINREGISTERREQUEST', Data, Req, {logged_in, LoggedInUsername}) ->
    try
        auth_service:verify_super_admin(LoggedInUsername),
        {adminregisterrequest, Username} = hrp_pb:decode_adminregisterrequest(Data),
        return_admin_register_response(Username, Req, {logged_in, LoggedInUsername})
    catch
        _:_ ->
            return_admin_list_failed_response(Req, {logged_in, LoggedInUsername})
    end;
handle_request('ADMINUPDATEREQUEST', Data, Req, {logged_in, LoggedInUsername}) ->
    try
        auth_service:verify_super_admin(LoggedInUsername),
        {adminupdaterequest, Username, Password, SuperAdmin, ResetPassword} = hrp_pb:decode_adminupdaterequest(Data),
        return_admin_update_response(Username, Password, SuperAdmin, ResetPassword, Req, {logged_in, LoggedInUsername})
    catch
        _:_ ->
            return_admin_list_failed_response(Req, {logged_in, LoggedInUsername})
    end;
handle_request('ADMINLISTREQUEST', Data, Req, {logged_in, LoggedInUsername}) ->
    try
        auth_service:verify_super_admin(LoggedInUsername),
        {adminlistrequest} = hrp_pb:decode_adminlistrequest(Data),
        return_admin_list_response(Req, {logged_in, LoggedInUsername})
    catch
        _:_ ->
            return_admin_list_failed_response(Req, {logged_in, LoggedInUsername})
    end;
handle_request('ADMINDELETEREQUEST', Data, Req, {logged_in, LoggedInUsername}) ->
    try
        auth_service:verify_super_admin(LoggedInUsername),
        {admindeleterequest, Username} = hrp_pb:decode_admindeleterequest(Data),
        return_admin_delete_response(Username, Req, {logged_in, LoggedInUsername})
    catch
        _:_ ->
            return_admin_list_failed_response(Req, {logged_in, LoggedInUsername})
    end;
handle_request('ADMINREGISTERREQUEST', _Data, Req, State) ->
    return_admin_list_failed_response(Req, State);
handle_request('ADMINUPDATEREQUEST', _Data, Req, State) ->
    return_admin_list_failed_response(Req, State);
handle_request('ADMINLISTREQUEST', _Data, Req, State) ->
    return_admin_list_failed_response(Req, State);
handle_request('ADMINDELETEREQUEST', _Data, Req, State) ->
    return_admin_list_failed_response(Req, State);
handle_request(_, _Data, Req, _State) ->
    {ok, Req, invalidrequest}.

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

-spec get_message_from_wrapper(iolist()) -> tuple().
get_message_from_wrapper(Msg) ->
    MessageWrapper =  hrp_pb:delimited_decode_wrapper(iolist_to_binary(Msg)),
    {[{wrapper, Type, Data} | _], _} = MessageWrapper,
    {Type, Data}.

-spec get_full_graph() -> list().
get_full_graph() ->
    get_wrapped_message(
        'GRAPHUPDATERESPONSE',
        hrp_pb:encode(
            {graphupdateresponse, node_graph_manager:get_graph_updates(0)}
        )
    ).

-spec return_admin_list_failed_response(any(), any()) -> any().
return_admin_list_failed_response(Req, State) ->
    {reply, {binary, get_wrapped_message(
        'ADMINLISTRESPONSE', hrp_pb:encode({adminlistresponse, 'FAILED', [], undefined}))
    }, Req, State}.

-spec return_admin_register_response(list(), any(), any()) -> any().
return_admin_register_response(Username, Req, State) ->
    try persistence_service:insert_admin(Username) of
        _ ->
            {Username, Password, false} = persistence_service:select_admin(Username),
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode(
                    {adminlistresponse, 'SUCCES', get_all_admins_for_list_response(), Password}
                ))}, Req, State}
    catch
        error:usernametaken ->
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode({adminlistresponse, 'USERNAME_TAKEN', [], undefined}))
            }, Req, State};
        _:_ ->
            return_admin_list_failed_response(Req, State)
    end.

-spec return_admin_update_response(list(), any(), boolean(), boolean(), any(), any()) -> any().
return_admin_update_response(Username, Password, SuperAdmin, ResetPassword, Req, State) ->
    try
        persistence_service:update_admin(Username, Password, SuperAdmin, ResetPassword),
        case ResetPassword of
            true ->
                {Username, NewPassword, SuperAdmin} = persistence_service:select_admin(Username);
            false ->
                NewPassword = undefined
        end,
        {reply, {binary, get_wrapped_message(
            'ADMINLISTRESPONSE', hrp_pb:encode(
                {adminlistresponse, 'SUCCES', get_all_admins_for_list_response(), NewPassword}
            ))}, Req, State}
    catch
        error:noremainingsuperadmin ->
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode({adminlistresponse, 'LAST_SUPERADMIN', [], undefined}))
            }, Req, State};
        error:invalidpassword ->
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode({adminlistresponse, 'INVALID_PASSWORD', [], undefined}))
            }, Req, State};
        _:_ ->
            return_admin_list_failed_response(Req, State)
    end.

-spec return_admin_list_response(any(), any()) -> any().
return_admin_list_response(Req, State) ->
    {reply, {binary, get_wrapped_message('ADMINLISTRESPONSE', hrp_pb:encode({
        adminlistresponse, 'SUCCES', get_all_admins_for_list_response(), undefined}))}, Req, State}.

-spec return_admin_delete_response(list(), any(), any()) -> any().
return_admin_delete_response(Username, Req, State) ->
    try persistence_service:delete_admin(Username) of
        _ ->
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode(
                    {adminlistresponse, 'SUCCES', get_all_admins_for_list_response(), undefined}
                ))}, Req, State}
    catch
        error:noremainingsuperadmin ->
            {reply, {binary, get_wrapped_message(
                'ADMINLISTRESPONSE', hrp_pb:encode({adminlistresponse, 'LAST_SUPERADMIN', [], undefined}))
            }, Req, State};
        _:_ ->
            return_admin_list_failed_response(Req, State)
    end.

-spec get_all_admins_for_list_response() -> list().
get_all_admins_for_list_response() ->
    [{admin, Username, SuperAdmin} ||
        {Username, SuperAdmin} <- persistence_service:select_all_admins()].



