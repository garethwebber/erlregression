-module(fac).
-export([main/1]).

% Handle command line arguments
main([A]) ->
  I = list_to_integer(atom_to_list(A)),
  F = fac(I),
  io:format("factorial of ~w is ~w~n", [I, F]),
  init:stop().

% The actual business logic
fac(0) -> 1;
fac(N) -> N * fac(N-1).
