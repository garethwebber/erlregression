-module(regression_graph).
-export([create_graph/3,create_graph_binary/3]).

create_graph(Points, A, B) ->
  Bin = create_graph_binary(Points, A, B),
  egd:save(Bin, "output.png"),
  egd:destroy(Bin),
  io:format("Graph created~n", []).

create_graph_binary(Points, A, B) ->
  Size = 600,
  Margin = 20,

  Im = egd:create(Size, Size),
  
  MaxMin = get_max_min(Points),
  draw_axes(Im, Size, Margin, MaxMin), 
  lists:foreach(fun(H) -> plot_point(Im, Size, Margin, MaxMin, H) end, Points),
  plot_line(Im, Size, Margin, MaxMin, A, B),

  ImageData = egd:render(Im),
  egd:destroy(Im),
  ImageData.

draw_axes(Image, Size, Margin, MaxMin) ->
  Black = egd:color({0,0,0}),
  {MinX, MinY, MaxX, MaxY} = MaxMin,

  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, MinX, 0}),
                  translate_point(MaxMin, Size, Margin, {point, MaxX, 0}),
                  Black), 
  egd:line(Image, translate_point(MaxMin, Size, Margin, {point, 0, MinY}),
                  translate_point(MaxMin, Size, Margin, {point, 0, MaxY}),
                  Black),
  draw_ticks(Image, Size, Margin, MaxMin),
  Image.

draw_ticks(Image, Size, Margin, MaxMin) ->
  Ticks = 10,

  {MinX, MinY, MaxX, MaxY} = MaxMin,
  TicksX = (MaxX - MinX) / Ticks,
  TicksY = (MaxY - MinY) / Ticks,

  case code:priv_dir(regression_app) of
        {error, bad_name} ->
            PrivDir = "priv";
        PrivDir ->
            ok
  end,
  Font = egd_font:load(filename:join([PrivDir, "6x11_latin1.wingsfont"])),

  plot_tick_x(Font, MinX, TicksX, Image, Size, Margin, MaxMin, Ticks + 1),
  plot_tick_y(Font, MinY, TicksY, Image, Size, Margin, MaxMin, Ticks + 1),
  Image.

plot_tick_x(_Font, _Start, _Gap, Image, _Size, _Margin, _MaxMin, 0) ->
  Image;

plot_tick_x(Font, Start, Gap, Image, Size, Margin, MaxMin, Count) ->
  Black = egd:color({0,0,0}),
  Point = {point, Start, 0},

  {CentreX, CentreY} = translate_point(MaxMin, Size, Margin, Point),
  egd:line(Image, {CentreX, CentreY}, {CentreX, CentreY + 5}, Black),
  egd:text(Image, {CentreX - 10, CentreY + 10}, Font, 
             number_to_string(Start), Black),
  plot_tick_x(Font, Start + Gap, Gap, Image, Size, Margin, MaxMin, Count -1).

plot_tick_y(_Font, _Start, _Gap, Image, _Size, _Margin, _MaxMin, 0) ->
  Image;

plot_tick_y(Font, Start, Gap, Image, Size, Margin, MaxMin, Count) ->
  Black = egd:color({0,0,0}),
  Point = {point, 0, Start},

  {CentreX, CentreY} = translate_point(MaxMin, Size, Margin, Point),
  egd:line(Image, {CentreX, CentreY}, {CentreX - 5, CentreY}, Black),
  egd:text(Image, {CentreX - 30, CentreY -10}, Font,
             number_to_string(Start), Black),
  plot_tick_y(Font, Start + Gap, Gap, Image, Size, Margin, MaxMin, Count -1).

number_to_string(Number) ->
  % This is an evil function
  try float_to_list(Number, [{decimals, 1}]) of 
    _   -> float_to_list(Number, [{decimals, 1}]) 
  catch
    _:_ -> integer_to_list(Number) 
  end.

plot_point(Image, Size, Margin, MaxMin, Point) ->
  Blue = egd:color({0,0,255}),
  {CentreX, CentreY} = translate_point(MaxMin, Size, Margin, Point),

  egd:line(Image, {CentreX - 5, CentreY - 5}, {CentreX +5, CentreY +5}, Blue),
  egd:line(Image, {CentreX - 5, CentreY + 5}, {CentreX +5, CentreY -5}, Blue),
  %io:format("Point (~w, ~w)~n", [CentreX, CentreY]),
  Image.

plot_line(Image, Size, Margin, MaxMin, A, B) ->
	Red = egd:color({255,0,0}),
  	{MinX, _, MaxX, _} = MaxMin,

	Left =  {point, MinX, A + (B * MinX)},
  Right =	{point, MaxX, A + (B * MaxX)},

	egd:line(Image, translate_point(MaxMin, Size, Margin, Left),
		        translate_point(MaxMin, Size, Margin, Right), Red),
	Image.

get_max_min(Points) ->
   get_max_min_ac(0, 0, 0, 0, Points).

get_max_min_ac(MinX, MinY, MaxX, MaxY, []) ->
	{MinX, MinY, MaxX, MaxY};

get_max_min_ac(MinX, MinY, MaxX, MaxY, Points) ->
	[H|T] = Points,
	{point, X, Y} = H,
	get_max_min_ac(min(X, MinX), min(Y, MinY), max(X, MaxX), max(Y, MaxY), T).

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
