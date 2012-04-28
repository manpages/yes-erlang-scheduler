-module(fission_syn).

-export([
	get/1,
	get_v/1,
	get_def/2,
	
	set/2,
	
	get_set/2,
	get_set_v/2,
	
	set_nx/2,
	
	del/1,
	
	inc/2,
	inc/1,
	inc_v/2,
	inc_v/1,
	
	run/2,
	run_v/2,
	
	spawn/2,
	spawn_v/2,
	
	run_def/3,
	run_set/2,
	run_set_v/2,
	
	cmd/2,
	cmd_v/2,
	
	def/2
]).

get(Key) ->
	fission_asyn:recv(fission_asyn:get(Key)).
	
get_v(Key) ->
	{value, Value} = fission_syn:get(Key),
	Value.
	
get_def(Key, Default) ->
	case fission_syn:get(Key) of
		{value, Value} ->
			Value;
		false ->
			Default
	end.
	
set(Key, Value) ->
	fission_asyn:recv(fission_asyn:set(Key, Value)).
	
get_set(Key, Value) ->
	fission_asyn:recv(fission_asyn:get_set(Key, Value)).
	
get_set_v(Key, Value) ->
	{value, RValue} = get_set(Key, Value),
	RValue.
	
set_nx(Key, Value) ->
	fission_asyn:recv(fission_asyn:set_nx(Key, Value)).
	
inc(Key, Value) ->
	fission_asyn:recv(fission_asyn:inc(Key, Value)).
	
inc(Key) ->
	fission_asyn:recv(fission_asyn:inc(Key)).
	
inc_v(Key, Value) ->
	{result, RValue} = fission_asyn:recv(fission_asyn:inc(Key, Value)),
	RValue.
	
inc_v(Key) ->
	{result, RValue} = fission_asyn:recv(fission_asyn:inc(Key)),
	RValue.
	
del(Key) ->
	fission_asyn:recv(fission_asyn:del(Key)).
	
run(Key, Fun) ->
	fission_asyn:recv(fission_asyn:run(Key, Fun)).
	
run_v(Key, Fun) ->
	{result, RValue} = run(Key, Fun),
	RValue.
	
spawn(Key, Fun) ->
	fission_asyn:recv(fission_asyn:spawn(Key, Fun)).
	
spawn_v(Key, Fun) ->
	{result, RValue} = fission_syn:spawn(Key, Fun),
	RValue.
	
run_def(Key, Fun, Default) ->
	case run(Key, Fun) of
		{result, RValue} ->
			RValue;
		false ->
			Default
	end.
	
run_set(Key, Fun) ->
	fission_asyn:recv(fission_asyn:run_set(Key, Fun)).
	
run_set_v(Key, Fun) ->
	{result, RValue} = run_set(Key, Fun),
	RValue.


cmd(Key, Cmd) ->
	fission_asyn:recv(fission_asyn:cmd(Key, Cmd)).
cmd_v(Key, Cmd) ->
	{_, RValue} = cmd(Key, Cmd),
	RValue.


def({_, V}, _) ->
	V;
def(_, D) ->
	D.