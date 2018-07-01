-module(regression_rest_point_handler).

-export([init/2, rest_init/2, trails/0, allowed_methods/2,content_types_provided/2,content_types_accepted/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

trails() ->
  Metadata =
    #{get =>
      #{tags => ["points"],
        description => "Gets all points in database",
        produces => ["text/plain"]
      }
    },
  [trails:trail("/rest/point",?MODULE, [], Metadata)].

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"PUT">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, getall(Req, State)},
      {<<"text/html">>, getall(Req, State)}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, addpoint(Req, State)},
      {<<"text/html">>, addpoint(Req, State)}
     ], Req, State}.

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
			Value = jiffy:encode(convert_point_to_ejson(Point));
		_ -> 
			Value = <<"{\"Error\": \"Addpoint\"}">>
	end,	

        {Value, Req, Opts}.

getall(Req, State) ->
	whereis(regression_app) ! {self(), "getpoints"},
	receive
		{_, List} -> List
	end,
        Data = jiffy:encode(convert_list_to_ejson(List)),

	Req1 = cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json">>
	}, Data, Req),
	{Data, Req1, State}.

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
