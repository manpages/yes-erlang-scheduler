-module(fission_test).

-export([
	benchmark/0,
	benchmark/1,
	
	split_to_16/0
]).

benchmark() ->
	benchmark(1000000).
	
benchmark(Count) ->
	run_test({set, same_value}, Count),
	run_test(get, Count),
	run_test(del, Count).

run_test(Cmd, Count) ->
	io:format("~n~p ~p:~n", [Count, Cmd]),
	T = fission_util:msec(),
	test(Cmd, Count, Count),
	D = fission_util:msec() - T,
	io:format("~p / sec (~p ms total)~n", [round(Count / D * 1000), D]).
	
test(_, 0, 0) ->
	ok;
test(Cmd, 0, Recv) ->
	%io:format("~p ~p~n", [0, Recv]),
	receive
		{res, {benchmark, I}, _} ->
			%io:format("< ~p ", [I]),
			test(Cmd, 0, Recv - 1);
		E ->
			io:format("? ~p~n", [E]),
			test(Cmd, 0, Recv - 1)
	end;
test(Cmd, Send, Recv) ->
	%io:format("~p ~p~n", [Send, Recv]),
	receive
		{res, {benchmark, I}, _} ->
			%io:format("< ~p ", [I]),
			test(Cmd, Send, Recv - 1);
		E ->
			io:format("? ~p~n", [E]),
			test(Cmd, Send, Recv - 1)
		after 0 ->
			fission ! {cmd, {benchmark, Send}, Cmd, {self(), {benchmark, Send}}},
			test(Cmd, Send - 1, Recv)
	end.
	

split_to_16() ->
	fission:split(0, 0),
	fission:split(0, 2),
	fission:split(1, 2),
	fission:split(2, 2),
	fission:split(3, 2).