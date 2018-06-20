-module(regression_graph).
-export([create_graph/0]).

create_graph() ->
  Size = 600,
  Im = egd:create(Size, Size),
  draw_axes(Im, Size, 20), 
  Bin = egd:render(Im),
  egd:save(Bin, "output.png"),
  io:format("Graph created~n", []).

draw_axes(Image, Size, Margin) ->
  Black = egd:color({0,0,0}),
  MaxMin = get_max_min(),
  {MinX, MinY, MaxX, MaxY} = MaxMin,

  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, MinX, 0}),
                  translate_point(MaxMin, Size, Margin, {point, MaxX, 0}),
                  Black), 
  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, 0, MinY}),
                  translate_point(MaxMin, Size, Margin, {point, 0, MaxY}),
                  Black),
  Image.

get_max_min() ->
   % return a hard-code grid for now
   {-5, -1, 5, 5}.

translate_point(MaxMin, Size, Margin, Point) ->
  {MinX, MinY, MaxX, MaxY} = MaxMin,
  {point, X, Y} = Point,

  RSize = Size - (2 * Margin), 
  RangeX = MaxX - MinX,
  RangeY = MaxY - MinY,
 
  % Y is reversed as EGD has 0,0 top left not bottom left. 
  CalcX = trunc((((float(X) - float(MinX)) / RangeX) * RSize) + Margin),
  CalcY = Size - trunc((((float(Y) - float(MinY)) / RangeY) * RSize) + Margin),
  
%  io:format("~w, ~w -> ~w, ~w~n", [X, Y, CalcX, CalcY]),
  {CalcX, CalcY}.
