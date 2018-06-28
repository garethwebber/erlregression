-module(regression_rest_handler).

-export([init/2, allowed_methods/2,content_types_provided/2,function_selector/2]).

-record(state, {op}).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, function_selector},
      {<<"text/plain">>, function_selector}
     ], Req, State}.

function_selector(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        debug ->
            debug(Req, State)
    end,
    {Body, Req1, State1}.

debug(Req0, Opts) ->
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
	[#{<<"point">> =>
	      #{<<"x">> => X, 
		<<"y">> => Y}
	  }].
