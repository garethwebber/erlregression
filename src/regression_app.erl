-module(regression_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    DB = regression_db:start(),
    Pid = spawn_link(fun() -> loop(DB) end),
    register(regression_app, Pid),
    {ok, Pid}.

stop(_State) ->
    ok.

loop(DB) ->
  receive
    {Pid, "loadpoint", Point} ->
        regression_db:insert_point(DB, Point),
        Pid ! {self(), ok},
        loop(DB);

    {Pid, "loadfile", File} ->
        Terms = regression_file:load_file_points(File),
        regression_db:insert_list(DB, Terms),
	Pid ! {self(), ok},
	loop(DB);

    {Pid, "runregression"} ->
        Terms = regression_db:get_all(DB),
	{regression, A, B} = regression_math:regression(Terms),
	regression_graph:create_graph(Terms, A, B),
	Pid ! {self(), ok},
	loop(DB);

    {Pid, "debug"} ->
        regression_db:debug(DB),
	Pid ! {self(), ok},
	loop(DB);
	  
    stop ->
       true
  end.
