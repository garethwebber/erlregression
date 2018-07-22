-module(regression_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Spin up the App Server
    regression_server:start(),

    % Spin up the Web Server
    Handlers = [
		regression_rest_point_handler,
		regression_rest_regression_handler,
		regression_rest_graph_handler,
		cowboy_swagger_handler
	       ],
    Trails = [ 
 	       {"/static/[...]", cowboy_static, 
		       {priv_dir, regression_app, "static"}},
               {"/favicon.ico", cowboy_static,
                       {priv_file, regression_app, "static/favicon.ico"}},
	       {"/", cowboy_static, 
		       {priv_file, regression_app, "static/index.html"}}
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

    io:format("Regression App servers running, now exiting. ~n", []),
    {ok, self()}.

stop(_State) ->
    ok.

