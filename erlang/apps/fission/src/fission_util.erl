-module(fission_util).
-export([
	to_int/1,
	to_list/1,
	to_binary/1,
	
	apply/2,
	
	get_def/2,
	
	unixtime/0,
	unixtime_float/0,
	msec/0,
	
	log/1,
	log/2,
	printmsg/0,
	dump_block/2
]).

to_int(Value) when is_integer(Value) ->
	Value;
to_int(Value) when is_list(Value) ->
	list_to_integer(Value);
to_int(Value) when is_binary(Value) ->
	list_to_integer(binary_to_list(Value)).
	
to_list(Value) when is_list(Value) ->
	Value;
to_list(Value) when is_integer(Value) ->
	integer_to_list(Value);
to_list(Value) when is_binary(Value) ->
	binary_to_list(Value);
to_list(Value) ->
	.io_lib:format("~p", [Value]).

to_binary(Value) when is_binary(Value) ->
	Value;
to_binary(Value) when is_list(Value) ->
	list_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
	list_to_binary(integer_to_list(Value));
to_binary(Value) ->
	list_to_binary(.io_lib:format("~p", [Value])).
	
apply({Module, Function}, Arg) ->
	erlang:apply(Module, Function, [Arg]);
apply({Module, Function, Args}, Arg) ->
	erlang:apply(Module, Function, [Arg|Args]);
apply(Fun, Arg) ->
	Fun(Arg).
	
get_def({value, Value}, _) ->
	Value;
get_def({result, Value}, _) ->
	Value;
get_def(_, Default) ->
	Default.
	
unixtime() ->
	{Mega, Secs, _} = erlang:now(),
	Mega * 1000000 + Secs.
	
msec() ->
	{Mega, Secs, MSec} = erlang:now(),
	Mega * 1000000000 + Secs * 1000 + MSec div 1000.

log(Format) ->
	log(Format, []).
log(Format, Params) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = .calendar:local_time(),
	Msg = io_lib:format("~p ~4..0B.~2..0B.~2..0B ~2..0B:~2..0B:~2..0B ~p " ++ Format ++ "~n", [msec(), Year, Month, Day, Hour, Minute, Second, node() | Params]),
	case get(log) of
		undefined ->
			fission ! {log, Msg};
		FID ->
			file:write(FID, Msg)
	end.
	
unixtime_float() ->
	{Mega, Secs, MSec} = erlang:now(),
	Mega * 1000000 + Secs + MSec / 1000000.
	
printmsg() ->
	receive
		Msg ->
			.io:fwrite("~p~n", [Msg]),
			printmsg()
	after 100 ->
		ok
	end.

dump_block(P, S) ->
	fission ! {table_dump, P, S, self()},
	read_dump(.gb_trees:empty()).
	
read_dump(Tree) ->
	receive
		{set, K, V} ->
			read_dump(.gb_trees:insert(K, V, Tree));
		{set_counter, _} ->
			.gb_trees:to_list(Tree)
	end.