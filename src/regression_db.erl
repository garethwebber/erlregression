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

delete_point(DB, Point) ->
	return ets:delete_object(DB, Point);

debug(DB) ->
	"Database contains: ~n~n" ++  debug_h(DB, get_all(DB)).

debug_h(DB, []) ->
	"";

debug_h(DB, List) -> 
	[H|T] = List,
	{point, PX, PY} = H,
        "(" ++ list_encode(PX) ++ ", " ++ list_encode(PY) ++ ")~n" ++ debug_h(DB, T).	

list_encode(Value) ->
	case Value of
		A when is_integer(A) ->
			integer_to_list(A);
		A when is_float(A) ->
			float_to_list(A)
	end.
