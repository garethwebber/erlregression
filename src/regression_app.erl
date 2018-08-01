-module(regression_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Spin up REST endpoint handling web-server 
    Handlers = [
    regression_rest_point_handler,
    regression_rest_regression_handler,
    regression_rest_graph_handler,
    cowboy_swagger_handler
         ],
    Trails = [ 
         {"/static/[...]", cowboy_static, {priv_dir, regression_app, "static"}},
         {"/favicon.ico", cowboy_static,  {priv_file, regression_app, "static/favicon.ico"}},
         {"/architecture-diagram.png", cowboy_static, {priv_file, regression_app, "architecture-diagram.png"}},
         {"/about", cowboy_static,        {priv_file, regression_app, "static/index.html"}},
         {"/", cowboy_static,             {priv_file, regression_app, "static/index.html"}}
         | trails:trails(Handlers)
  ],
    trails:store(Trails),

    case os:getenv("PORT") of
        false ->
            Port = 8080; 
        Other ->
            Port = list_to_integer(Other)
    end, 

    Dispatch = trails:single_host_compile(Trails),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
         env => #{dispatch => Dispatch}
    }),
    io:format("Started REST API via Cowboy on port: ~w~n", [Port]),

    % Bring up supervisor: supervisor will start regression app-server.
    regression_sup:start_link().

stop(_State) ->
    ok.

