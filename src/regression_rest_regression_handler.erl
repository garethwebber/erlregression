-module(regression_rest_regression_handler).
-behaviour(trails_handler).
-export([init/2, rest_init/2, trails/0, allowed_methods/2,
	 content_types_provided/2,resource_exists/2,
	 runregression/2]).

init(Req, Opts) ->
       {cowboy_rest, Req, Opts}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

trails() ->
  Metadata =
    #{get =>
      #{tags => ["Run Regression"],
        description => "Runs the linear regression",
        produces => ["application/json"]
      }
    },
  [trails:trail("/rest/regression",?MODULE, [], Metadata)].

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, runregression}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_exists(Req, _State) ->
	{true, Req, index}.

runregression(Req, State) ->
	whereis(regression_app) ! {self(), "runregression"},
	receive
		{_, Value} -> Value 
	end,
	{regression, A, B} = Value,
	Return = #{<<"regression">> =>
                     #{<<"A">> => A,
		       <<"B">> => B} 
          },
	{jiffy:encode(Return), Req, State}.

