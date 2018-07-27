-module(regression_rest_point_handler).
-behaviour(trails_handler).
-export([init/2, rest_init/2, trails/0, allowed_methods/2,delete_resource/2,
	 content_types_provided/2,content_types_accepted/2,resource_exists/2,
	 getall/2, addpoint/2]).

init(Req, Opts) ->
       {cowboy_rest, Req, Opts}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

trails() ->
  Metadata = #{
      get =>
      #{tags => ["Manage data"],
        description => "Gets all points in database",
        produces => ["application/json"]
      },
      delete =>
      #{tags => ["Manage data"], 
        description => "Delete a point from the database",
        parameters => [
	   #{name => <<"point">>,
           in => <<"path">>,
           required => true,
	   type=><<"string">>
	    }
	]
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
	    example => <<"[
 			   { \"point\": {
      				\"y\": -3,
      				\"x\": -4
    			     }
                           },
  			   { \"point\": {
      				\"y\": 3,
      				\"x\": -2
    			     }
                           }]">>,
            type => <<"string">>}
        ]
      }
    },
  [trails:trail("/rest/point[/:point]",?MODULE, [], Metadata)].

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
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

delete_resource(Req, State) ->
    Text = cowboy_req:binding(point, Req),
    List = [list_to_integer(I) || I <- string:tokens(binary_to_list(Text), ",")],
    [X, Y] = List,
    Point = {point, X, Y},
    regression_server:delete_point(Point),
    {true, Req, State}.

resource_exists(Req, _State) ->
	{true, Req, index}.

addpoint(Req, State) ->
	{ok, RawBody, Req1} = cowboy_req:read_body(Req),
        Body = jiffy:decode(RawBody),
	Points = convert_ejson_to_list(Body),
	regression_server:load_list(Points),

        {true, Req1, State}.

getall(Req, State) ->
        List = regression_server:get_points(),
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

