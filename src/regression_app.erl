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

    Pid = spawn_link(fun() -> loop() end),
    register(regression_app, Pid),    
    io:format("Regression App running: ~w~n", [Pid]),
    {ok, Pid}.

stop(_State) ->
    ok.

loop() ->
  receive
    {Pid, "loadpoint", Point} ->
        regression_server:load_point(Point),
        Pid ! {self(), ok},
        loop();

    {Pid, "loadlist", List} ->
        regression_server:load_list(List),
        Pid ! {self(), ok},
        loop();

    {Pid, "loadfile", File} ->
        regression_server:load_file(File),
        Pid ! {self(), ok},
	loop();

    {Pid, "getpoints"} ->
	Pid ! {self(), regression_server:get_points()},
	loop();

    {Pid, "runregression"} ->
	Pid ! {self(), regression_server:run_regression()},
	loop();

    {Pid, "getgraph"} ->
        Pid ! {self(), regression_server:get_graph()},
        loop();

    {Pid, "debug"} ->
        io:format("Dubug~n",[]),
	Pid ! {self(), regression_server:debug()},
	loop();
	  
    stop ->
       true
  end.
