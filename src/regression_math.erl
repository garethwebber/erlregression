-module(regression_math).
-export([run_regression/0]).

run_regression() ->
  Filename = "points.dat",

  case code:priv_dir(my_application) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account 
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            ok
    end,

  {ok, Terms} = file:consult(filename:join([PrivDir, Filename])),
  lists:map(fun printPoint/1, Terms),
  {regression, A, B} = regression(Terms),
  io:format("A=~w B=~w ~n", [A, B]).

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
