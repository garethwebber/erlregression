-module(regression_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Spin up the points database
    DB = regression_db:start(),

    % Spin up the App Server
    Pid = spawn_link(fun() -> loop(DB) end),
    register(regression_app, Pid),

    % Spin up the Web Server
    Handlers = [
		regression_rest_point_handler,
		regression_rest_regression_handler,
		regression_rest_graph_handler,
		cowboy_swagger_handler
	       ],
    Trails = [ 
 	       {"/static/[...]", cowboy_static, 
		       {priv_dir, regression_app, "static/"}},
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
    io:format("Starting Cowboy on port: ~w~n", [Port]),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
         env => #{dispatch => Dispatch}
    }),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),

    {ok, Pid}.

stop(_State) ->
    ok.

loop(DB) ->
  receive
    {Pid, "loadpoint", Point} ->
        regression_db:insert_point(DB, Point),
        Pid ! {self(), ok},
        loop(DB);

    {Pid, "loadlist", List} ->
        regression_db:insert_list(DB, List),
        Pid ! {self(), ok},
        loop(DB);

    {Pid, "loadfile", File} ->
        Terms = regression_file:load_file_points(File),
        regression_db:insert_list(DB, Terms),
	Pid ! {self(), ok},
	loop(DB);

    {Pid, "getpoints"} ->
	Pid ! {self(), regression_db:get_all(DB)},
	loop(DB);

    {Pid, "runregression"} ->
        Terms = regression_db:get_all(DB),
	Value = regression_math:regression(Terms),
	Pid ! {self(), Value},
	loop(DB);

    {Pid, "getgraph"} ->
        Terms = regression_db:get_all(DB),
        {regression, A, B} = regression_math:regression(Terms),
        Bin = regression_graph:create_graph_binary(Terms, A, B),
        Pid ! {self(), Bin},
        loop(DB);

    {Pid, "debug"} ->
	Return = regression_db:debug(DB),
	Pid ! {self(), Return},
	loop(DB);
	  
    stop ->
       true
  end.
