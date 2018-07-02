-module(regression_rest_graph_handler).
-behaviour(trails_handler).
-export([init/2, rest_init/2, trails/0, allowed_methods/2,
	 content_types_provided/2,resource_exists/2,
	 getgraph/2]).

init(Req, Opts) ->
       {cowboy_rest, Req, Opts}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

trails() ->
  Metadata =
    #{get =>
      #{tags => ["Graphics"],
        description => "Gets a graph of the points and the regression",
        produces => ["image/png"]
      }
    },
  [trails:trail("/rest/graph",?MODULE, [], Metadata)].

content_types_provided(Req, State) ->
    {[
      {{<<"image">>, <<"png">>, []}, getgraph}
     ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_exists(Req, _State) ->
	{true, Req, index}.

getgraph(Req, State) ->
	whereis(regression_app) ! {self(), "getgraph"},
	receive
		{_, Value} -> Value 
	end,
	error_logger:info_msg("getgraph."),
	{Value, Req, State}.

