-module(regression_static_handler).

-export([init/2, allowed_methods/2,content_types_provided/2,function_selector/2]).

-record(state, {op}).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, function_selector}
     ], Req, State}.

function_selector(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
        index ->
           index(Req, State)
    end,
    {Body, Req1, State1}.

index(Req0, Opts) ->
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
}, <<"
<HTML>
<BODY>
     <H1>Regression Web App</H1>
     <P>Click here to see <a href=/rest/debug>database debug</a> content.
</BODY>	 
</HTML>">>, Req0),
	{ok, Req, Opts}.
