-module(fac).
-export([main/0]).

main() ->
  lists:map(fun printPoint/1,
            pointfile:loadFile("fac.dat")),
  init:stop().

printPoint({point,X,Y}) ->
  %F = fac(Number),
  %io:format("factorial of ~w is ~w~n", [Number, F])
  io:format("(~w, ~w)~n", [X, Y]).

% The actual business logic
fac(0) -> 1;
fac(N) -> N * fac(N-1).
