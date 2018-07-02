-module(regression_rest_point_handler).
-behaviour(trails_handler).
-export([init/2, rest_init/2, trails/0, allowed_methods/2,
	 content_types_provided/2,content_types_accepted/2,resource_exists/2,
	 getall/2, addpoint/2]).

init(Req, Opts) ->
       {cowboy_rest, Req, Opts}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

trails() ->
  Metadata =
    #{get =>
      #{tags => ["Manage data"],
        description => "Gets all points in database",
        produces => ["application/json"]
      },
      put =>
      #{tags => ["Manage data"],
        description => "Add a point to the database",
        produces => ["application/json"],
        parameters => [
          #{name => <<"x">>,
            description => <<"X co-ordinate">>,
            in => <<"path">>,
            required => true,
            type => <<"string">>},
          #{name => <<"y">>,
            description => <<"Y co-ordinate">>,
            in => <<"path">>,
            required => true,
            type => <<"string">>}
        ]
      }
    },
  [trails:trail("/rest/point",?MODULE, [], Metadata)].

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, addpoint}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, getall}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

resource_exists(Req, _State) ->
	{true, Req, index}.

addpoint(Req, State) ->
	error_logger:info_msg("addpoint"),
%	Points = cowboy_req:match_qs([x, y], Req),
%	{_, X} = maps:find(x, Points),
%	{_, Y} = maps:find(y, Points),
%	Point = {point, bin_to_num(X), 
%			bin_to_num(Y)},
Point = {point, 1, 3},
	whereis(regression_app) ! {self(), "loadpoint", Point},
        receive
		{_, Resp} -> Resp	
        end,

	case Resp of
		ok -> 
			Data = jiffy:encode(convert_point_to_ejson(Point));
		_ -> 
			Data = <<"{\"Error\": \"Addpoint\"}">>
	end,

       % Req1 = cowboy_req:reply(200, #{
       %         <<"content-type">> => <<"application/json">>
       % }, Data, Req),
%	Resp = cowboy_req:set_resp_body(Data, Req),
	error_logger:info_msg("addpoint: ", Data),
        {true, Req, State}.

getall(Req, State) ->
	whereis(regression_app) ! {self(), "getpoints"},
	receive
		{_, List} -> List
	end,
        Data = jiffy:encode(convert_list_to_ejson(List)),

	error_logger:info_msg("getall: ", Data),
	{Data, Req, State}.

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
