-module(regression_math).
-export([regression/1]).

% The actual business logic
regression(L) ->
  N = length(L),
  {regressionterms, X, Y, XY, X2, Y2} = terms(L),
  % io:format("X=~w Y=~w XY=~w X2=~w Y2=~w N=~w ~n", [X, Y, XY, X2, Y2, N]),
  A = (Y*X2-X*XY) / (N*X2-X2),
  B = (N*XY - X*Y) / (N*X2-X2),
  {regression, A, B}.

% Create sums of X, Y, XY, X square and Y squared from a list of points 
terms(L) ->
  {X, Y, XY, X2, Y2} = terms_array(L),
  {regressionterms, lists:sum(X), lists:sum(Y), lists:sum(XY),
                    lists:sum(X2), lists:sum(Y2)}. 

% Take list of tuples. Take each point in term and create five
% new lists of X, Y, X *Y and X square and Y squared.
terms_array(L) ->
  terms_array_acc(L, [], [], [], [], []).

terms_array_acc([H|T], X, Y, XY, X2, Y2) ->
  case H of
    {point,PX,PY} -> terms_array_acc(T, [PX|X], [PY|Y], [PX*PY|XY],
                                        [PX*PX|X2], [PY*PY|Y2])
  end;
terms_array_acc([],    X, Y, XY, X2, Y2) -> {X, Y, XY, X2, Y2}.
