-module(regression_db).
-export([start/0, insert_point/2, insert_list/2, get_all/1, debug/1]).

start() ->
	ets:new(regression_app, [duplicate_bag, named_table, public]).

insert_point(DB, Point) ->
	ets:insert(DB, Point),
	true.

insert_list(_, []) ->
        true;

insert_list(DB, List) ->
	[H|T] = List,
	insert_point(DB, H),
	insert_list(DB, T).

get_all(DB) ->
	ets:tab2list(DB).

debug(DB) ->
	io:format("Database contains: ~n~n"),
        print_list(get_all(DB)).

print_list([]) ->
        true;

print_list(List) ->
	[H|T] = List,
	print_point(H),
	print_list(T).

print_point({point,PX,PY}) ->
  io:format("(~w, ~w)~n", [PX, PY]).
