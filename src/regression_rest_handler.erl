-module(regression_rest_handler).

-export([init/2, allowed_methods/2,content_types_provided/2,function_selector/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, function_selector},
      {<<"text/html">>, function_selector}
     ], Req, State}.

function_selector(Req, State) ->
    Path = cowboy_req:path(Req),
    {Body, Req1, State1} = case Path of
	   <<"/rest/addpoint">> -> addpoint(Req, State);
	   <<"/rest/debug">> ->    debug(Req, State)
    end,
    {Body, Req1, State1}.

addpoint(Req, Opts) ->
	Points = cowboy_req:match_qs([x, y], Req),
	{_, X} = maps:find(x, Points),
	{_, Y} = maps:find(y, Points),
	Point = {point, bin_to_num(X), 
			bin_to_num(Y)},

	whereis(regression_app) ! {self(), "loadpoint", Point},
        receive
		{_, Resp} -> Resp	
        end,

	case Resp of
		ok -> 
			Req1 = cowboy_req:reply(200, #{
                	<<"content-type">> => <<"application/json">>
        		}, <<"{\"Action\" : \"Completed\"}">>, Req);
		_ -> 
			Req1 = cowboy_req:reply(500, #{
                            <<"content-type">> => <<"application/json">>
                            }, <<"{\"Error\": \"Addpoint\"}">>, Req)
	end,	

        {ok, Req1, Opts}.

debug(Req0, Opts) ->
	whereis(regression_app) ! {self(), "getpoints"},
	receive
		{_, List} -> List
	end,
        Data = convert_list_to_ejson(List),
        JsonData = jiffy:encode(Data, [pretty]),

	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, JsonData, Req0),
	{ok, Req, Opts}.

convert_list_to_ejson([]) ->
	[];

convert_list_to_ejson(List) ->
	[H|T] = List,
	convert_point_to_ejson(H) ++ convert_list_to_ejson(T).

convert_point_to_ejson(Point) ->
	{point, X, Y} = Point,
	[#{<<"point">> =>
	      #{<<"x">> => X, 
		<<"y">> => Y}
	  }].

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.
