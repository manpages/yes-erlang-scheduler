-module(fission_once).

-export([
	request/2,
	request/3,
	
	request_to/3,
	request_to/4,
	
	wait/2,
	wait/3,
	wait/4,
	
	wait_v/2,
	wait_v/3,
	wait_v/4,
	
	expect/1,
	expect/2,
	expect/3,
	
	expect_v/1,
	expect_v/2,
	expect_v/3,
	
	send_to/4,
	
	get_r/1,
	get_to/2,
	get/1,
	get_v/1,
	get_e/1,
	get_ev/1
]).


request(Key, Cmd) ->
	request(Key, Cmd, {Key, Cmd}).
	
request(Key, Cmd, Ref) ->
	case erlang:get({fission_once, Ref}) of
		undefined ->
			sendreq(Key, Cmd, Ref);
		_ ->
			ok
	end.
	
	
request_to(Key, Cmd, PID) ->
	request_to(Key, Cmd, {Key, Cmd}, PID).
	
request_to(Key, Cmd, Ref, PID) ->
	sendreq(Key, Cmd, Ref, PID).


wait(Key, Cmd) ->
	wait(Key, Cmd, {Key, Cmd}, 1000).
	
wait(Key, Cmd, Ref) ->
	wait(Key, Cmd, Ref, 1000).
	
wait(Key, Cmd, Ref, Timeout) ->
	case erlang:get({fission_once, Ref}) of
		undefined ->
			receive
				{res, Ref, Answer} ->
					put({fission_once, Ref}, {value, Answer}),
					Answer
			after 0 ->
				sendreq(Key, Cmd, Ref),
				recv(Ref, Timeout)
			end;
		wait ->
			recv(Ref, Timeout);
		{value, Val} ->
			Val
	end.


wait_v(Key, Cmd) ->
	wait_v(Key, Cmd, {Key, Cmd}, 1000).
	
wait_v(Key, Cmd, Ref) ->
	wait_v(Key, Cmd, Ref, 1000).
	
wait_v(Key, Cmd, Ref, Timeout) ->
	{_, Value} = wait(Key, Cmd, Ref, Timeout),
	Value.
	
	
expect(Ref) ->
	expect(Ref, 1000).
	
expect(Ref, Timeout) when is_integer(Timeout) ->
	case erlang:get({fission_once, Ref}) of
		{value, Val} ->
			Val;
		_ ->
			recv(Ref, Timeout)
	end;
	
expect(Key, Cmd) ->
	expect({Key, Cmd}, 1000).
	
expect(Key, Cmd, Timeout) ->
	expect({Key, Cmd}, Timeout).
	

expect_v(Ref) ->
	expect_v(Ref, 1000).
	
expect_v(Ref, Timeout) when is_integer(Timeout) ->
	{_, Value} = expect(Ref, Timeout),
	Value;
	
expect_v(Key, Cmd) ->
	expect_v({Key, Cmd}, 1000).
	
expect_v(Key, Cmd, Timeout) ->
	expect_v({Key, Cmd}, Timeout).
	

get_r(Key) ->
	request(Key, get).
	
get_to(Key, PID) ->
	request_to(Key, get, PID).
	
get(Key) ->
	wait(Key, get).
	
get_v(Key) ->
	wait_v(Key, get).
	
get_e(Key) ->
	expect(Key, get).
	
get_ev(Key) ->
	expect_v(Key, get).
	

send_to(Ref, Cmd, Value, PID) ->
	PID ! {res, {Ref, Cmd}, {result, Value}}.


sendreq(Key, Cmd, Ref) ->
	sendreq(Key, Cmd, Ref, self()).

sendreq(Key, Cmd, Ref, PID) ->
	fission ! {cmd, Key, Cmd, {PID, Ref}},
	put({fission_once, Ref}, wait),
	ok.


recv(Ref, Timeout) ->
	receive
		{res, Ref, Answer} ->
			put({fission_once, Ref}, {value, Answer}),
			Answer;
		{error, Ref, EType, Err, Trace} ->
			put({fission_once, Ref}, {fission_error, EType, Err, Trace}),
			.erlang:error({fission_error, EType, Err, Trace})
	after Timeout ->
		.erlang:error({fission_noreply, Ref})
	end.