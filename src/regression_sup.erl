-module(regression_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RegressionServer = {regression_server, {regression_server, start_link, []},
		        permanent, brutal_kill, worker, [regression_server]},

    Children = [RegressionServer],
    
    {ok, {{one_for_one, 10000, 1}, Children}}.
