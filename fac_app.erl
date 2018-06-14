-module(fac_app).
-behaviour(application).
-export([start/2, stop/1, run_regression/0]).

start(_Type, _Args) ->
    Pid = spawn_link(fun loop/0),
    register(regression, Pid),
    {ok, Pid}.

stop(_State) ->
    ok.

loop() ->
  receive
    {Pid, "runregression"} ->
       Pid ! {self(), ok},
       run_regression(),
       loop();
    stop ->
       true
  end.

run_regression() ->
  {ok, Terms} = file:consult("points.dat"),
  lists:map(fun printPoint/1, Terms),
  {regression, A, B} = regression(Terms),
  io:format("A=~w B=~w ~n", [A, B]),
%  createGraph(),
  init:stop().

createGraph() ->
  Im = egd:create(600, 600),
  drawAxes(Im, 600), 
  Bin = egd:render(Im),
  egd:save(Bin, "output.png").

drawAxes(Image, Size) ->
  Margin = 20,
  OMargin = Size - Margin,
  Black = egd:color({0,0,0}),

  egd:line(Image, {Margin, OMargin}, {Margin,  Margin}, Black),
  egd:line(Image, {Margin, OMargin}, {OMargin, OMargin}, Black),
  Image.

printPoint({point,PX,PY}) ->
  io:format("(~w, ~w)~n", [PX, PY]).

% The actual business logic
regression(L) ->
  N = length(L),
  {regressionterms, X, Y, XY, X2, Y2} = terms(L),
  io:format("X=~w Y=~w XY=~w X2=~w Y2=~w N=~w ~n", [X, Y, XY, X2, Y2, N]),
  A = (Y*X2-X*XY) / (N*X2-X2),
  B = (N*XY - X*Y) / (N*X2-X2),
  {regression, A, B}.

% terms_array_acc build arrays of x, y, xy, x^2 and y^2
% terms then creates a tuple of the sums in the same order
terms(L) ->
  {X, Y, XY, X2, Y2} = terms_array(L),
  {regressionterms, lists:sum(X), lists:sum(Y), lists:sum(XY),
                    lists:sum(X2), lists:sum(Y2)}. 

terms_array(L) ->
  terms_array_acc(L, [], [], [], [], []).

terms_array_acc([H|T], X, Y, XY, X2, Y2) ->
  case H of
    {point,PX,PY} -> terms_array_acc(T, [PX|X], [PY|Y], [PX*PY|XY],
                                        [PX*PX|X2], [PY*PY|Y2])
  end;
terms_array_acc([],    X, Y, XY, X2, Y2) -> {X, Y, XY, X2, Y2}.
