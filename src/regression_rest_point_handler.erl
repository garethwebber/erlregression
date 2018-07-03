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
          #{name => <<"Request Body">>,
            description => <<"Request body as JSON">>,
	    in => body,
            required => true,
            type => <<"string">>}
        ]
      }
    },
  [trails:trail("/rest/point",?MODULE, [], Metadata)].

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, addpoint},
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, addpoint}
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
	{ok, RawBody, Req1} = cowboy_req:read_body(Req),
        Body = jiffy:decode(RawBody),
	Points = convert_ejson_to_list(Body),
	whereis(regression_app) ! {self(), "loadlist", Points},
        receive
		{_, Resp} -> Resp	
        end,

	error_logger:info_msg("addpoint: ", Points),
        {true, Req1, State}.

getall(Req, State) ->
	whereis(regression_app) ! {self(), "getpoints"},
	receive
		{_, List} -> List
	end,
        Data = jiffy:encode(convert_list_to_ejson(List)),

	{Data, Req, State}.

convert_ejson_to_list([]) ->
	[];

convert_ejson_to_list(List) ->
	[H|T] = List,
	convert_ejson_to_point(H) ++ convert_ejson_to_list(T).

convert_ejson_to_point(Point) ->
        % This is evil but works
	Values = case Point of 
	    {[{<<"point">>,{[{<<"y">>,Y},{<<"x">>,X}]}}]} -> {Y, X};
	    {[{<<"point">>,{[{<<"x">>,X},{<<"y">>,Y}]}}]} -> {Y, X}	
	end,
	{Y, X} = Values,
	Return = {point, X, Y},
	[Return].

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

