-module(regression_graph).
-export([create_graph/0]).

create_graph() ->
  Im = egd:create(600, 600),
  draw_axes(Im, 600), 
  Bin = egd:render(Im),
  egd:save(Bin, "output.png"),
  io:format("Graph created~n", []).

draw_axes(Image, Size) ->
  Margin = 20,
  OMargin = Size - Margin,
  Black = egd:color({0,0,0}),

  egd:line(Image, {Margin, OMargin}, {Margin,  Margin}, Black),
  egd:line(Image, {Margin, OMargin}, {OMargin, OMargin}, Black),
  Image.
