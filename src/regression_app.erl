-module(regression_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Pid = spawn_link(fun loop/0),
    register(regression_app, Pid),
    {ok, Pid}.

stop(_State) ->
    ok.

loop() ->
  receive
    {Pid, "runregression"} ->
       Pid ! {self(), ok},
       regression_file:run_regression("points.dat"),
       regression_graph:create_graph(),
       loop();
    stop ->
       true
  end.
