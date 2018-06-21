-module(regression_graph).
-export([create_graph/1]).

create_graph(Points) ->
  Size = 600,
  Margin = 20,

  Im = egd:create(Size, Size),
  
  MaxMin = get_max_min(),
  draw_axes(Im, Size, Margin, MaxMin), 
  lists:foreach(fun(H) -> plot_point(Im, Size, Margin, MaxMin, H) end, Points),

  Bin = egd:render(Im),
  egd:save(Bin, "output.png"),
  io:format("Graph created~n", []).

draw_axes(Image, Size, Margin, MaxMin) ->
  Black = egd:color({0,0,0}),
  {MinX, MinY, MaxX, MaxY} = MaxMin,

  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, MinX, 0}),
                  translate_point(MaxMin, Size, Margin, {point, MaxX, 0}),
                  Black), 
  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, 0, MinY}),
                  translate_point(MaxMin, Size, Margin, {point, 0, MaxY}),
                  Black),
  Image.

plot_point(Image, Size, Margin, MaxMin, Point) ->
  Blue = egd:color({0,0,255}),
  {CentreX, CentreY} = translate_point(MaxMin, Size, Margin, Point),

  egd:line(Image, {CentreX - 5, CentreY - 5}, {CentreX +5, CentreY +5}, Blue),
  egd:line(Image, {CentreX - 5, CentreY + 5}, {CentreX +5, CentreY -5}, Blue),
  %io:format("Point (~w, ~w)~n", [CentreX, CentreY]),
  Image.

get_max_min() ->
   % return a hard-code grid for now
   {-5, -5, 5, 25}.

translate_point(MaxMin, Size, Margin, Point) ->
  {MinX, MinY, MaxX, MaxY} = MaxMin,
  {point, X, Y} = Point,

  RSize = Size - (2 * Margin), 
  RangeX = MaxX - MinX,
  RangeY = MaxY - MinY,
 
  % Y is reversed as EGD has 0,0 top left not bottom left. 
  CalcX = trunc((((float(X) - float(MinX)) / RangeX) * RSize) + Margin),
  CalcY = Size - trunc((((float(Y) - float(MinY)) / RangeY) * RSize) + Margin),
  
  %io:format("~w, ~w -> ~w, ~w~n", [X, Y, CalcX, CalcY]),
  {CalcX, CalcY}.
