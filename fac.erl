-module(fac).
-export([main/0]).

main() ->
  lists:map(fun printFac/1,
            facfile:loadFile("fac.dat")),
  init:stop().

printFac(Number) ->
  F = fac(Number),
  io:format("factorial of ~w is ~w~n", [Number, F]).

% The actual business logic
fac(0) -> 1;
fac(N) -> N * fac(N-1).
