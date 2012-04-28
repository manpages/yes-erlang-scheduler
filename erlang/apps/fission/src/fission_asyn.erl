-module(fission_asyn).

-export([
	get/1,
	get/2,
	
	set/2,
	set/3,
	set_n/2,
	
	% deprecated
	get_set/2,
	set_nx/2,
	set_nx_n/2,
	inc/2,
	inc/1,
	
	del/1,
	del/2,
	del_n/1,
	
	run/2,
	run/3,
	run_n/2,
	
	spawn/2,
	spawn/3,
	spawn_n/2,
	
	run_set/2,
	run_set/3,
	run_set_n/2,
	
	cmd/2,
	cmd/3,
	cmd_n/2,
	cmd_from/3,
	
	recv/1,
	recv/2,
	recv_v/1,
	recv_v/2
]).


get(Key) ->
	fission ! {cmd, Key, get, {self(), Ref = make_ref()}},
	Ref.
	
get(Key, Ref) ->
	fission ! {cmd, Key, get, {self(), Ref}},
	Ref.
	
set(Key, Value) ->
	fission ! {cmd, Key, {set, Value}, {self(), Ref = make_ref()}},
	Ref.
	
set(Key, Value, Ref) ->
	fission ! {cmd, Key, {set, Value}, {self(), Ref}},
	Ref.
	
set_n(Key, Value) ->
	fission ! {cmd, Key, {set, Value}, false},
	ok.
	
% TODO replace get_set, set_nx & inc with run_sets, remove from table code
	
get_set(Key, Value) ->
	fission ! {cmd, Key, {get_set, Value}, {self(), Ref = make_ref()}},
	Ref.
	
set_nx(Key, Value) ->
	fission ! {cmd, Key, {set_nx, Value}, {self(), Ref = make_ref()}},
	Ref.
	
set_nx_n(Key, Value) ->
	fission ! {cmd, Key, {set_nx, Value}, false},
	ok.
	
inc(Key, Value) ->
	fission ! {cmd, Key, {inc, Value}, {self(), Ref = make_ref()}},
	Ref.
	
inc(Key) ->
	fission ! {cmd, Key, {inc, 1}, {self(), Ref = make_ref()}},
	Ref.
	
del(Key) ->
	fission ! {cmd, Key, del, {self(), Ref = make_ref()}},
	Ref.
	
del(Key, Ref) ->
	fission ! {cmd, Key, del, {self(), Ref}},
	Ref.
	
del_n(Key) ->
	fission ! {cmd, Key, del, false},
	ok.
	
run(Key, Fun) ->
	fission ! {cmd, Key, {run, Fun}, {self(), Ref = make_ref()}},
	Ref.
	
run(Key, Fun, Ref) ->
	fission ! {cmd, Key, {run, Fun}, {self(), Ref}},
	Ref.
	
run_n(Key, Fun) ->
	fission ! {cmd, Key, {run, Fun}, false},
	ok.
	
spawn(Key, Fun) ->
	fission ! {cmd, Key, {spawn, Fun}, {self(), Ref = make_ref()}},
	Ref.
	
spawn(Key, Fun, Ref) ->
	fission ! {cmd, Key, {spawn, Fun}, {self(), Ref}},
	Ref.
	
spawn_n(Key, Fun) ->
	fission ! {cmd, Key, {spawn, Fun}, false},
	ok.
	
run_set(Key, Fun) ->
	fission ! {cmd, Key, {run_set, Fun}, {self(), Ref = make_ref()}},
	Ref.
	
run_set(Key, Fun, Ref) ->
	fission ! {cmd, Key, {run_set, Fun}, {self(), Ref}},
	Ref.
	
run_set_n(Key, Fun) ->
	fission ! {cmd, Key, {run_set, Fun}, false},
	ok.


cmd(Key, Cmd) ->
	fission ! {cmd, Key, Cmd, {self(), Ref = make_ref()}},
	Ref.
	
cmd(Key, Cmd, Ref) ->
	fission ! {cmd, Key, Cmd, {self(), Ref}},
	ok.
	
cmd_n(Key, Cmd) ->
	fission ! {cmd, Key, Cmd, false},
	ok.

cmd_from(Key, Cmd, From) ->
	fission ! {cmd, Key, Cmd, From},
	ok.
	

recv(Ref) ->
	recv(Ref, 500).
	
recv(Ref, Timeout) ->
	receive
		{res, Ref, Answer} ->
			Answer;
		{error, Ref, EType, Err, Trace} ->
			.erlang:error({fission_error, EType, Err, Trace})
	after Timeout ->
		.erlang:error({fission_noreply, Ref})
	end.
	
recv_v(Ref) ->
	{_, Value} = recv(Ref),
	Value.

recv_v(Ref, Timeout) ->
	{_, Value} = recv(Ref, Timeout),
	Value.
