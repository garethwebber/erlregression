-module(regression_web_handler).

-export([init/2]).

init(Req0, Opts) ->
	whereis(regression_app) ! {self(), "debug"},
	receive
		{Pid, List} -> List
	end,
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, list_to_binary(List), Req0),
	{ok, Req, Opts}.
