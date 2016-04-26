-module(websocket_app).  
-behaviour(application).  
-export([start/2, stop/1]).  
  
start(_StartType, _StartArgs) ->  
  Dispatch = cowboy_router:compile([  
      {'_', [  
        {"/", cowboy_static, {priv_file, websocket, "index.html"}},  
        {"/websocket", websocket_handler, []},
        {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
      ]}  
    ]),  
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],  
        [{env, [{dispatch, Dispatch}]}]),  
      websocket_sup:start_link().

stop(_State) ->  
    ok.