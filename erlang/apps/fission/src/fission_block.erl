-module(fission_block).

-export([
	new/0,
	
	create/3,
	init/3,
	init_split/5,
	
	work/1,
	writer/2,
	writer/3,
	
	rewriter/4,
	
	spawn_proc/3
]).

-record(state, {
	
	path,
	shift,
	role,
	
	table,
	
	counter=0,
	
	writer,
	rewriter,
	aof=ok
	
}).

-define(HPS, H, H bsr S, S). % hash, path, shift

new() ->
	{t, u, u, u, u}.

%
%	TABLE OPS
%

atime(Hash) ->
	(fission_util:unixtime() + (Hash rem 3600)) div 3600.

set_r(Node, Key, Value, Hash, Path, Shift) ->
	N = (Path band 3) + 2,
	NP = Path bsr 2,
	E = element(N, Node),
	R = case E of
		{t, _, _, _, _} ->
			set_r(E, Key, Value, Hash, NP, Shift + 2);
		u ->
			{k, Hash, Key, Value, atime(Hash)};
		{k, OHash, OKey, OValue, _} ->
			case OKey of
				Key ->
					{k, Hash, Key, Value, atime(Hash)};
				_ ->
					case OHash of
						Hash ->
							{l, Hash, atime(Hash), [{OKey, OValue}, {Key, Value}]};
						_ ->
							push_r(
								push_r(
									{t, u, u, u, u},
									{k, Hash, Key, Value, atime(Hash)}, Hash, NP, Shift + 2
								),
								E, OHash, OHash bsr (Shift + 2), Shift + 2
							)
					end
			end;
		{l, OHash, _, List} ->
			case OHash of
				Hash ->
					case .lists:keytake(Key, 1, List) of
						false ->
							{l, Hash, atime(Hash), [{Key, Value} | List]};
						{value, _, NList} ->
							{l, Hash, atime(Hash), [{Key, Value} | NList]}
					end;
				_ ->
					push_r(
						push_r(
							{t, u, u, u, u},
							{k, Hash, Key, Value, atime(Hash)}, Hash, NP, Shift + 2
						),
						E, OHash, OHash bsr (Shift + 2), Shift + 2
					)
			end
	end,
	setelement(N, Node, R).

%	push_r places an element in the tree with a Hash that is definitely not present (see usage above)
%	actually, better idea would be to take both elements, find a point in path where they differ and jump right there. but this is decent too, and also more universal
push_r(Node, SE, Hash, Path, Shift) ->
	N = (Path band 3) + 2,
	NP = Path bsr 2,
	E = element(N, Node),
	R = case E of
		{t, _, _, _, _} ->
			push_r(E, SE, Hash, NP, Shift + 2);
		u ->
			SE;
		_ ->
			OHash = element(2, E),
			push_r(
				push_r(
					{t, u, u, u, u},
					SE, Hash, NP, Shift + 2
				),
				E, OHash, OHash bsr (Shift + 2), Shift + 2
			)
	end,
	setelement(N, Node, R).
	
get_r(Node, Key, Hash, Path, Shift) ->
	N = (Path band 3) + 2,
	E = element(N, Node),
	case E of
		{t, _, _, _, _} ->
			{NE, R} = get_r(E, Key, Hash, Path bsr 2, Shift + 2),
			{
				case NE of
					false ->
						false;
					_ ->
						setelement(N, Node, NE)
				end,
				R
			};
		{k, Hash, Key, Value, OAtime} ->
			{
				case Atime = atime(Hash) of
					OAtime ->
						false;
					_ ->
						setelement(N, Node, {k, Hash, Key, Value, Atime})
				end,
				{value, Value}
			};
		{l, Hash, OAtime, List} ->
			{
				case Atime = atime(Hash) of
					OAtime ->
						false;
					_ ->
						setelement(N, Node, {l, Hash, Atime, List})
				end,
				case .lists:keyfind(Key, 1, List) of
					{Key, Value} ->
						{value, Value};
					_ ->
						false
				end
			};
		_ ->
			{false, false}
	end.
	
del_r(Node, Key, Hash, Path, Shift) ->
	N = (Path band 3) + 2,
	E = element(N, Node),
	R = case E of
		{t, _, _, _, _} ->
			del_r(E, Key, Hash, Path bsr 2, Shift + 2);
		u ->
			u;
		{k, _, OKey, _, _} ->
			case OKey of
				Key ->
					u;
				_ ->
					E
			end;
		{l, OHash, _, List} ->
			case OHash of
				Hash ->
					case .lists:keytake(Key, 1, List) of
						false ->
							E;
						{value, _, NList} ->
							{l, OHash, atime(OHash), NList}
					end;
				_ ->
					E
			end
	end,
	RT = setelement(N, Node, R),
	case RT of
		{t, u, u, u, u} ->
			u;
		_ ->
			RT
	end.
	
%
%	TABLE COMMANDS
%

get(Node, Key, Hash, Path, Shift) ->
	{NN, Res} = get_r(Node, Key, Hash, Path, Shift),
	case NN of
		false ->
			{Node, Res};
		_ ->
			{NN, Res}
	end.
	
set(Node, Key, Value, Hash, Path, Shift) ->
	{set_r(Node, Key, Value, Hash, Path, Shift), ok}.

get_set(Node, Key, Value, Hash, Path, Shift) ->
	% TODO optimize
	{GetNode, Res} = get(Node, Key, Hash, Path, Shift),
	NewNode = set_r(GetNode, Key, Value, Hash, Path, Shift),
	{NewNode, Res}.
	
set_nx(Node, Key, Value, Hash, Path, Shift) ->
	% TODO optimize
	{GetNode, Res} = get(Node, Key, Hash, Path, Shift),
	case Res of
		false ->
			NewNode = set_r(GetNode, Key, Value, Hash, Path, Shift),
			{NewNode, true};
		_ ->
			{GetNode, false}
	end.
	
del(Node, Key, Hash, Path, Shift) ->
	NewNode = case del_r(Node, Key, Hash, Path, Shift) of
		u -> {t, u, u, u, u};
		DelNode -> DelNode
	end,
	{NewNode, ok}.
	
inc(Node, Key, Value, Hash, Path, Shift) ->
	% TODO optimize
	{GetNode, Res} = get(Node, Key, Hash, Path, Shift),
	NewValue = case Res of
		{value, Num} when is_number(Num) ->
			Num + Value;
		_ ->
			Value
	end,
	NewNode = set_r(GetNode, Key, NewValue, Hash, Path, Shift),
	{NewNode, {result, NewValue}}.
	
run(Node, Key, Fun, Hash, Path, Shift) ->
	{GetNode, GetRes} = get(Node, Key, Hash, Path, Shift),
	Res = fission_util:apply(Fun, GetRes),
	{GetNode, {result, Res}}.

run_set(Node, Key, Fun, Hash, Path, Shift) ->
	% TODO optimize
	{GetNode, GetRes} = get(Node, Key, Hash, Path, Shift),
	{NewValue, Res} = fission_util:apply(Fun, GetRes),
	NewNode = set_r(GetNode, Key, NewValue, Hash, Path, Shift),
	{NewNode, {result, Res}}.
	
spawn_cmd(Node, Key, Fun, From, Hash, Path, Shift) ->
	{GetNode, GetRes} = get(Node, Key, Hash, Path, Shift),
	spawn(?MODULE, spawn_proc, [GetRes, Fun, From]),
	{GetNode, ok}.
	
spawn_proc(GetRes, Fun, From) ->
	try
		case fission_util:apply(Fun, GetRes) of
			{chain, Key, Cmd} ->
				fission ! {cmd, Key, Cmd, From};
			{result, Res} ->
				spawn_send_res(Res, From);
			Res ->
				spawn_send_res(Res, From)
		end
	catch
		EType:Err ->
			case From of
				{EP, ER} when is_pid(EP) ->
					EP ! {error, ER, EType, Err, .erlang:get_stacktrace()};
				_ ->
					fission_util:log("~w:~w in anonymous spawn of ~w: ~w", [EType, Err, Fun, .erlang:get_stacktrace()])
			end
	end.
	
spawn_send_res(Res, {PID, Ref}) ->
	PID ! {res, Ref, {result, Res}};
spawn_send_res(_, _) ->
	ok.
	
%
%	GARBAGE COLLECTOR
%

dig_r(Node, Path) ->
	case Node of
		{t, _, _, _, _} ->
			dig_r(element((Path band 3) + 2, Node), Path bsr 2);
		_ ->
			Node
	end.

garbage_find({t, u, u, u, u}) ->
	ok;
garbage_find(Table) ->
	garbage_find(Table, 100, .gb_sets:empty()). % collect up to 100 keys of garbage
garbage_find(_, 0, _) ->
	ok;
garbage_find(Table, C, Done) ->
	E = dig_r(Table, .random:uniform(16#FFFFFFFF)),
	case E of
		{k, Hash, Key, _Value, Atime} ->
			case .gb_sets:is_element(Hash, Done) of
				false ->
					NAT = atime(Hash),
					if
						NAT - Atime > -1 ->
							.io:format("Unload ~p~n (atime ~p)~n", [Key, Atime]),
							garbage_find(Table, C - 1, .gb_sets:insert(Hash, Done));
						true ->
							ok % hit non-expired key, stop for now
					end;
				true ->
					ok % already hit this key, surely time to stop
			end;
		{l, Hash, Atime, _List} ->
			case .gb_sets:is_element(Hash, Done) of
				false ->
					NAT = atime(Hash),
					if
						NAT - Atime > -1 ->
							.io:format("Unload ~p~n (atime ~p)~n", [Hash, Atime]),
							garbage_find(Table, C - 1, .gb_sets:insert(Hash, Done));
						true ->
							ok % hit non-expired key, stop for now
					end;
				true ->
					ok % already hit this key, surely time to stop
			end;
		u ->
			garbage_find(Table, C - 1, Done) % try once more
	end.
%
%	TABLE INIT
%

create(Path, Shift, Role) ->
	spawn(?MODULE, init, [Path, Shift, Role]).

init(Path, Shift, Role) ->
	Table = new(),
	State = #state{path=Path, shift=Shift, table=Table, role=Role},
	.random:seed(.erlang:now()),
	catch register(list_to_atom("fission_" ++ integer_to_list(Path) ++ "_" ++ integer_to_list(Shift)), self()),
	case Role of
		master ->
			init_master(State);
		slave ->
			init_slave(State)
	end.
	
init_master(State) ->
	FN = getdir(State#state.path, State#state.shift) ++ "cur.aof",
	case .file:open(FN, [read, binary, raw]) of
		{ok, RFile} ->
			NewState = read_aof(State, RFile, State#state.path, State#state.shift),
			.file:close(RFile);
		_ ->
			NewState = State
	end,
	Writer = spawn(?MODULE, writer, [FN, append]),
	work(NewState#state{writer=Writer}).
	
init_slave(State) ->
	{Table, CC} = recv_dump(State#state.table, State#state.shift),
	Writer = spawn(?MODULE, writer, [getdir(State#state.path, State#state.shift) ++ "cur.aof", append]),
	NewState = State#state{table=Table, writer=Writer, counter=CC},
	work(rewriter_start(NewState)).
	
%
%	TABLE DUMP
%

table_dump({t, A, B, C, D}, PID) ->
	table_dump(A, PID),
	table_dump(B, PID),
	table_dump(C, PID),
	table_dump(D, PID);
table_dump(u, _) ->
	ok;
table_dump({l, _, _, List}, PID) ->
	.lists:foreach(fun ({Key, Value}) ->
		PID ! {set, Key, Value}
	end, List);
table_dump({k, _, Key, Value, _}, PID) ->
	PID ! {set, Key, Value}.
	
recv_dump(Table, S) ->
	receive
		{set, K, V} ->
			H = .erlang:phash2(K, 16#100000000),
			NT = set_r(Table, K, V, ?HPS),
			recv_dump(NT, S);
		{set_counter, CC} ->
			{Table, CC}
	end.

%
%	TABLE SPLIT
%

split(State) ->
	T = State#state.table,
	P = State#state.path,
	S = State#state.shift,
	R = State#state.role,
	CC = State#state.counter,
	{t, A, B, C, D} = case T of
		{t, _, _, _, _} ->
			T;
		{k, H, _, _, _} ->
			push_r({t, u, u, u, u}, T, ?HPS);
		{l, H, _, _} ->
			push_r({t, u, u, u, u}, T, ?HPS);
		u ->
			{t, u, u, u, u}
	end,
	{
		spawn(?MODULE, init_split, [A, P, S + 2, R, CC]),
		spawn(?MODULE, init_split, [B, P bor (1 bsl S), S + 2, R, CC]),
		spawn(?MODULE, init_split, [C, P bor (2 bsl S), S + 2, R, CC]),
		spawn(?MODULE, init_split, [D, P bor (3 bsl S), S + 2, R, CC])
	}.
	
init_split(Table, Path, Shift, Role, CC) ->
	.random:seed(.erlang:now()),
	Writer = spawn(?MODULE, writer, [getdir(Path, Shift) ++ "cur.aof", write]),
	Writer ! {pre, Path band (bnot (16#FFFFFFFF bsl (Shift - 2))), Shift - 2},
	work(#state{path=Path, shift=Shift, table=Table, writer=Writer, role=Role, counter=CC}).
%
%	GETDIR
%
	
getdir(Path, Shift) ->
	.file:make_dir("data"),
	.file:make_dir(D = "data/" ++ integer_to_list(Shift)),
	.lists:flatten(getdir_pp(D, Shift div 8 + 1, Path) ++ "/").
getdir_pp(R, 0, _) ->
	R;
getdir_pp(Pre, Lev, Path) ->
	P = Pre ++ "/" ++ .io_lib:format("~2.16.0b", [Path band 255]),
	.file:make_dir(P),
	getdir_pp(P, Lev - 1, Path bsr 8).
	
%
%	AOF READER
%
	
read_aof(State, File, Path, Shift) ->
	S = State#state.shift,
	Table = State#state.table,
	case .file:read(File, 5) of
		eof ->
			State;
		{ok, Data} ->
			<<10:8, Size:32>> = Data,
			{ok, E} = .file:read(File, Size),
			case binary_to_term(E) of
				{cmd, I, K, Cmd} ->
					H = .erlang:phash2(K, 16#100000000),
					KP = H band (bnot (16#FFFFFFFF bsl Shift)),
					%.io:format("~p ~p ~p~n", [EE, KP, Path]),
					NewState = case KP of
						Path ->
							{NewTable, _} = case Cmd of
								{set, V} ->
									set(Table, K, V, ?HPS);
								{inc, V} ->
									inc(Table, K, V, ?HPS);
								del ->
									del(Table, K, ?HPS);
								{run_set, F} ->
									run_set(Table, K, F, ?HPS)
							end,
							CC = I,
							State#state{table=NewTable, counter=CC};
						_ ->
							State
					end;
					
				{pre, RPath, RShift} ->
					RFN = getdir(RPath, RShift) ++ "cur.aof",
					case .file:open(RFN, [read, binary, raw]) of
						{ok, RFile} ->
							NewState = read_aof(State, RFile, Path, Shift),
							.file:close(RFile);
						_ ->
							NewState = State
					end
			end,
			read_aof(NewState, File, Path, Shift)
	end.
	
%
%	AOF WRITER
%
	
writer(FN, Mode) ->
	{ok, File} = .file:open(FN, [Mode, raw]),
	writer(File, 0, <<>>).
writer(File, Ticks, Buf) ->
	Timeout = case Ticks of
		0 ->
			5000;
		_ ->
			.erlang:max(500 - Ticks, 100)
	end,
	NewFile = receive
		
		W = {cmd, _, _, _} ->
			Bin = term_to_binary(W),
			Size = byte_size(Bin),
			NewTicks = case Ticks of
				5000 ->
					%.io:format("*"),
					.file:write(File, <<Buf/binary, 10, Size:32, Bin/binary>>),
					NewBuf = <<>>,
					0;
				_ ->
					NewBuf = <<Buf/binary, 10, Size:32, Bin/binary>>,
					Ticks + 1
			end,
			File;
			
		W = {pre, _, _} ->
			Bin = term_to_binary(W),
			Size = byte_size(Bin),
			.file:write(File, <<10, Size:32, Bin/binary>>),
			.file:datasync(File),
			NewTicks = 0,
			NewBuf = Buf,
			File;
			
		{tell_pos, Rewriter} ->
			.file:write(File, Buf),
			.file:datasync(File),
			{ok, Pos} = .file:position(File, eof),
			Rewriter ! {aof_pos, self(), Pos},
			NewTicks = 0,
			NewBuf = <<>>,
			File;
			
		{pause, Rewriter} ->
			.file:write(File, Buf),
			.file:close(File),
			Rewriter ! paused,
			NewTicks = 0,
			NewBuf = <<>>,
			receive
				{continue, NFN} ->
					{ok, NF} = .file:open(NFN, [append, raw]),
					NF
			end;
			
		die ->
			NewTicks = 0, % erlang made me do this ;(
			NewBuf = <<>>,
			.file:close(File),
			exit(normal)
		
	after Timeout ->
		case Ticks of
			0 ->
				ok;
			_ ->
				%.io:format("$"),
				.file:write(File, Buf)
		end,
		NewTicks = 0,
		NewBuf = <<>>,
		File
	end,
	?MODULE:writer(NewFile, NewTicks, NewBuf).
	
%
%	AOF REWRITER
%
	
rewriter_start(State) ->
	%TS = fission_util:unixtime_float(),
	Rewriter = spawn(?MODULE, rewriter, [self(), State#state.table, State#state.counter, getdir(State#state.path, State#state.shift)]),
	State#state.writer ! {tell_pos, Rewriter},
	%.io:format("Rewriter started (took ~f sec)~n", [fission_util:unixtime_float() - TS]),
	State#state{aof=rewriting, rewriter=Rewriter}.
	
rewriter(PID, Table, CC, Dir) ->
	CurFn = Dir ++ "cur.aof",
	RwFn = Dir ++ "rw.aof",
	{ok, File} = .file:open(RwFn, [write, raw]),
	rewriter_rt(Table, File, CC),
	receive
		{aof_pos, Writer, Pos} ->
			Writer ! {pause, self()},
			receive
				paused ->
					ok
			end
	end,
	{ok, OldFile} = .file:open(CurFn, [read, binary, raw]),
	{ok, Pos} = .file:position(OldFile, Pos),
	.file:copy(OldFile, File),
	.file:close(OldFile),
	.file:close(File),
	.file:rename(RwFn, CurFn),
	Writer ! {continue, CurFn},
	PID ! rewrite_done.
	
rewriter_rt({t, A, B, C, D}, File, CC) ->
	rewriter_rt(A, File, CC),
	rewriter_rt(B, File, CC),
	rewriter_rt(C, File, CC),
	rewriter_rt(D, File, CC);
rewriter_rt(u, _, _) ->
	ok;
rewriter_rt({l, _, _, List}, File, CC) ->
	.lists:foreach(fun ({Key, Value}) ->
		rewriter_rt({k, 0, Key, Value, 0}, File, CC)
	end, List);
rewriter_rt({k, _, Key, Value, _}, File, CC) ->
	Bin = term_to_binary({cmd, CC, Key, {set, Value}}),
	Size = byte_size(Bin),
	.file:write(File, <<10, Size:32, Bin/binary>>).
	
%
%	WORKER
%
	
work(State) ->
	Table = State#state.table,
	Timeout = case State#state.role of
		master ->
			infinity;%100; TODO when some dumping actually works
		slave ->
			infinity
	end,
	NewState = receive
	
		{cmd, K, Cmd, From} ->
			try
				H = .erlang:phash2(K, 16#100000000),
				S = State#state.shift,
				{NT, Answer} = case Cmd of
					get ->
						get(Table, K, ?HPS);
					{set, V} ->
						set(Table, K, V, ?HPS);
					{get_set, V} ->
						get_set(Table, K, V, ?HPS);
					{set_nx, V} ->
						set_nx(Table, K, V, ?HPS);
					del ->
						del(Table, K, ?HPS);
					{inc, V} when is_number(V) ->
						inc(Table, K, V, ?HPS);
					{run, F} ->
						run(Table, K, F, ?HPS);
					{run_set, F} ->
						run_set(Table, K, F, ?HPS);
					{spawn, F} ->
						spawn_cmd(Table, K, F, From, ?HPS)
				end,
				case From of
					{PID, Ref} ->
						case Cmd of 
							{spawn, _} ->
								ok;
							_ ->
								PID ! {res, Ref, Answer}
						end;
					_ ->
						ok
				end,
				WCmd = case Cmd of
					{set, _} ->
						Cmd;
					{get_set, WV} ->
						{set, WV};
					{set_nx, WV} ->
						{set, WV};
					del ->
						del;
					{inc, _} ->
						Cmd;
					{run_set, WF} ->
						case is_function(WF) andalso (erlang:fun_info(WF, type) == {type, local}) of
							true ->
								{_, WV} = get_r(NT, K, ?HPS), % making inefficient even more inefficient, since 2003. 
								{set, WV};
							false ->
								Cmd
						end;
					_ ->
						false
				end,
				case WCmd of
					false ->
						State#state{table=NT};
					_ ->
						NCC = State#state.counter + 1,
						State#state.writer ! {cmd, NCC, K, WCmd},
						State#state{table=NT, counter=NCC}
				end
			catch
				EType:Err ->
					case From of
						{EP, ER} when is_pid(EP) ->
							EP ! {error, ER, EType, Err, .erlang:get_stacktrace()};
						_ ->
							fission_util:log("~w:~w in anonymous ~w on ~w: ~w", [EType, Err, Cmd, K, .erlang:get_stacktrace()])
					end,
					State
			end;
			
		{set, K, V} -> % for initial replication only
			H = .erlang:phash2(K, 16#100000000),
			S = State#state.shift,
			NT = set_r(Table, K, V, ?HPS),
			NCC = State#state.counter + 1,
			State#state{table=NT, counter=NCC};
			
		{table_dump, PID} ->
			table_dump(Table, PID),
			PID ! {set_counter, State#state.counter},
			State;
			
		{set_counter, NCC} ->
			State#state{counter=NCC};
		
		get_counter ->
			fission ! {counter, State#state.path, State#state.shift, State#state.counter},
			State;
			
		rewrite_aof ->
			case State#state.aof of
				ok ->
					NS = rewriter_start(State),
					fission ! {rewrite_started, State#state.path, State#state.shift},
					NS;
				rewriting ->
					State
			end;
			
		rewrite_done ->
			fission ! {rewrite_done, State#state.path, State#state.shift},
			State#state{aof=ok};
			
		split ->
			case State#state.aof of
				rewriting ->
					receive
						rewrite_done ->
							ok
					end;
				ok ->
					ok
			end,
			State#state.writer ! die,
			Res = split(State),
			fission ! {split, State#state.path, State#state.shift, Res},
			exit(normal);
		
		code_change ->
			State#state.writer ! code_change,
			State;
			
		Wut ->
			.io:format("Unknown msg to table: ~p~n", [Wut]),
			State
			
		after Timeout ->
			garbage_find(Table),
			State
			
	end,
	?MODULE:work(NewState).