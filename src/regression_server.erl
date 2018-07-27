-module(regression_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([load_point/1, load_list/1, load_file/1,
	get_points/0, run_regression/0, get_graph/0, debug/0]).
-compile(export_all).
-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()       -> gen_server:call(?MODULE, stop).

load_point(Point) -> gen_server:call(?MODULE, {loadpoint, Point}).
delete_point(Point) -> gen_server:call(?MODULE, {deletepoint, Point}).
load_list(List)  -> gen_server:call(?MODULE, {loadlist,  List}).
load_file(File)   -> gen_server:call(?MODULE, {loadfile,  File}).
get_points()      -> gen_server:call(?MODULE, {getPoints}).
run_regression()  -> gen_server:call(?MODULE, {runRegression}).
get_graph()       -> gen_server:call(?MODULE, {getGraph}).
debug()          -> gen_server:call(?MODULE, {debug}).

init([]) ->
    % Spin up the points database
    PID = self(),
    DB = regression_db:start(),
    io:format("Regression server started (Process: ~w, ETS database: ~w)~n", [PID, DB]),
    {ok, DB}.

handle_call({loadpoint, Point}, _From, Tab) ->
        regression_db:insert_point(Tab, Point),
	{reply, ok, Tab};

handle_call({deletepoint, Point}, _From, Tab) ->
        regression_db:delete_point(Tab, Point),
        {reply, ok, Tab};

handle_call({loadlist, List}, _From, Tab) ->
        regression_db:insert_list(Tab, List),
        {reply, ok, Tab};

handle_call({loadfile, File}, _From, Tab) ->
        Terms = regression_file:load_file_points(File),
        regression_db:insert_list(Tab, Terms),
        {reply, ok, Tab};

handle_call({getPoints}, _From, Tab) ->
	Return = regression_db:get_all(Tab),
        {reply, Return, Tab};

handle_call({runRegression}, _From, Tab) ->
        Terms = regression_db:get_all(Tab),
	Value = regression_math:regression(Terms),
        {reply, Value, Tab};

handle_call({getGraph}, _From, Tab) ->
        Terms = regression_db:get_all(Tab),
        {regression, A, B} = regression_math:regression(Terms),
        Bin = regression_graph:create_graph_binary(Terms, A, B),
        {reply, Bin, Tab};

handle_call({debug}, _From, Tab) ->
	Return = regression_db:debug(Tab),
	{reply, Return, Tab};
 
handle_call(stop, _From, Tab) -> {stop, normal, stopped, Tab}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> {ok}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
