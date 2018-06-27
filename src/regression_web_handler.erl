-module(regression_web_handler).

-export([init/2]).

init(Req0, Opts) ->
	whereis(regression_app) ! {self(), "getpoints"},
	receive
		{Pid, List} -> List
	end,
        Data = convert_list_to_ejson(List),
        JsonData = jiffy:encode(Data, [pretty]),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, JsonData, Req0),
	{ok, Req, Opts}.

convert_list_to_ejson([]) ->
	[];

convert_list_to_ejson(List) ->
	[H|T] = List,
	convert_point_to_ejson(H) ++ convert_list_to_ejson(T).

convert_point_to_ejson(Point) ->
	{point, X, Y} = Point,
	[#{<<"point">> =>[#{<<"x">> => X}, #{<<"y">> => Y}]}].
