-module(websocket_app).
-behaviour(application).

%% API.
-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, websocket, "index.html"}},
      {"/map.html", cowboy_static, {priv_file, websocket, "map.html"}},
      {"/websocket", ws_handler, []},
      {
          "/static/js/hrp.proto", cowboy_static,
          {
              priv_file,
              websocket,
              "/static/js/hrp.proto",
              [{mimetypes, {<<"text">>, <<"plain">>, []}}]
          }
      },
      {
          "/static/[...]", cowboy_static,
          {
              priv_dir,
              websocket,
              "static",
              [{mimetypes, cow_mimetypes, all}]
          }
      }
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]),
  websocket_sup:start_link().

stop(_State) ->
  ok.