-module(regression_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RegressionApp = {regression_app, {regression_app, start_link, []},
		     permanent, 2000, worker, [regression_app]},

    Children = [RegressionApp],
    
    {ok, {{one_for_all, 0, 1}, []}}.
