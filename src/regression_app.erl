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
    Dispatch = cowboy_router:compile([
	{'_', [
		{"/rest/debug", regression_rest_handler, [debug]},
		{"/[...]", regression_static_handler, [index]}
	]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
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
	{regression, A, B} = regression_math:regression(Terms),
	regression_graph:create_graph(Terms, A, B),
	Pid ! {self(), ok},
	loop(DB);

    {Pid, "debug"} ->
	Return = regression_db:debug(DB),
	Pid ! {self(), Return},
	loop(DB);
	  
    stop ->
       true
  end.
