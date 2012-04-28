-module(fission).

-behaviour(application).


% Interface
-export([
	start/2,
	stop/1,
	
	get_table/0,
	print_table/1,
	
	initialize/1,
	split/2,
	set_master/3,
	set_slave/3
]).

% Inner functions
-export([
	init/0,
	worker/1
]).


-record(state, {
	version,
	table
}).


start(normal, _) ->
	PID = spawn(?MODULE, init, []),
	register(fission, PID),
	{ok, PID}.

stop(_) ->
	fission ! stop.

get_table() ->
	fission ! {get_table, self()},
	receive
		Table -> Table
	end.

print_table({table, Version, Table}) ->
	.io:format("Table version: ~p~n", [Version]),
	table_walk(Table, fun(Node, Path, Shift) ->
		.io:format("~*.. B: ~*.4.0B (~p) : ~p~n", [Shift, Shift, Shift div 2, Path, Path, Node]),
		Node
	end).

initialize(Node) ->
	{fission, Node} ! {table_change, Node, initialize}.

split(Path, Shift) ->
	{table, _Version, Table} = get_table(),
	{block, Master, _Slaves} = table_get(Table, Path, Shift),
	{fission, Master} ! {table_change, Master, {split, Path, Shift}}.

set_master(Path, Shift, Node) ->
	{table, _Version, Table} = get_table(),
	{block, Master, _Slaves} = table_get(Table, Path, Shift),
	{fission, Master} ! {table_change, Master, {set_master, Path, Shift, Node}}.

set_slave(Path, Shift, Node) ->
	{fission, Node} ! {table_change, Node, {set_slave, Path, Shift}}.




%% Implementation %%

read_table() ->
	DataDir = "data/", % ++ atom_to_list(node()) ++ "/",
	case .file:read_file(DataDir ++ "table.dat") of
		{ok, Data} ->
			binary_to_term(Data);
		
		{error, enoent} ->
			{0, undefined}
	end.

write_table({Version, Table}) ->
	DataDir = "data/", % ++ atom_to_list(node()) ++ "/",
	.file:make_dir("data/"),
	%.file:make_dir(DataDir),
	.file:write_file(DataDir ++ "table.dat", term_to_binary({Version, Table}, [compressed])).



table_set(_Table, _Path, 0, Value) ->
	Value;
table_set(Table, Path, Shift, Value) ->
	case Table of
		{tree, _, _, _, _} ->
			Child = element((Path band 3) + 2, Table),
			setelement((Path band 3) + 2, Table, table_set(Child, Path bsr 2, Shift - 2, Value));
		
		_ ->
			Value
	end.

table_get(Table, _Path, 0) ->
	Table;
table_get(Table, Path, Shift) ->
	case Table of
		{tree, _, _, _, _} ->
			Child = element((Path band 3) + 2, Table),
			table_get(Child, Path bsr 2, Shift - 2);
		
		Table ->
			Table
	end.

table_walk(Table, Fun) ->
	table_walk(Table, Fun, 0, 0).
table_walk(Table, Fun, Path, Shift) ->
	case Table of
		{tree, Elem1, Elem2, Elem3, Elem4} ->
			{tree,
				table_walk(Elem1, Fun, Path + (0 bsl Shift), Shift + 2),
				table_walk(Elem2, Fun, Path + (1 bsl Shift), Shift + 2),
				table_walk(Elem3, Fun, Path + (2 bsl Shift), Shift + 2),
				table_walk(Elem4, Fun, Path + (3 bsl Shift), Shift + 2)
			};
		
		undefined ->
			undefined;
		
		Table ->
			Fun(Table, Path, Shift)
	end.



init() ->
	%group_leader(whereis(init), self()),
	
	% Read config
	Config = case file:consult("fission.cfg") of
		{ok, C} -> C;
		_ -> []
	end,
	
	{ok, Log} = file:open("fission.log", [write]),
	put(log, Log),
	
	% Read table
	{Version, Table} = read_table(),
	fission_util:log("Starting at version ~B", [Version]),
	
	% Create tables
	State = #state{
		version = Version,
		table = table_walk(Table, fun
			({block, Master, Slaves}, Path, Shift) when is_pid(Master) ->
				{block, fission_block:create(Path, Shift, master), Slaves};
			
			({block, Master, Slaves}, Path, Shift) ->
				{block, Master, .lists:map(fun
					(Slave) when is_pid(Slave) ->
						{fission, Master} ! {table_dump, Path, Shift, PID = fission_block:create(Path, Shift, slave)},
						PID;
					(Slave) ->
						Slave
				end, Slaves)};
			
			(undefined, _Path, _Shift) ->
				undefined
		end)
	},
	
	
	% Broadcast our table to already connected nodes
	.rpc:abcast(nodes(), fission, {version, Version, node()}),
	
	% Try to connect to other nodes, if any
	.net_kernel:monitor_nodes(true),
	case .lists:keyfind(nodes, 1, Config) of
		{nodes, Nodes} when is_list(Nodes) ->
			.lists:foreach(fun net_kernel:connect_node/1, Nodes);
		
		false ->
			false
	end,
	
	worker(State).



receive_table_change(State) ->
	receive
		{table_change, Node, Change} ->
			receive_table_change(table_change(Node, Change, State))
	after 0 ->
		State
	end.

table_change(Node, Change, State) when Node == node() ->
	.global:trans({fission_table, node()}, fun() ->
		CurrentState = receive_table_change(State),
		Table = CurrentState#state.table,
		NewTable = case Change of
			initialize ->
				.rpc:sbcast(nodes(), fission, {table_change, Node, Change}),
				table_set(Table, 0, 0, {block, fission_block:create(0, 0, master), []});
			
			{split, Path, Shift} ->
				{block, Master, Slaves} = table_get(Table, Path, Shift),
				Master ! split,
				{Master1, Master2, Master3, Master4} = receive
					{split, Path, Shift, Res} ->
						Res
				end,
				.rpc:sbcast(nodes(), fission, {table_change, Node, Change}),
				table_set(Table, Path, Shift, {tree,
					{block, Master1, Slaves},
					{block, Master2, Slaves},
					{block, Master3, Slaves},
					{block, Master4, Slaves}
				});
			
			{set_master, Path, Shift} ->
				{block, Master, Slaves} = table_get(Table, Path, Shift),
				.rpc:sbcast(nodes(), fission, {table_change, Node, Change}),
				table_set(Table, Path, Shift, {block, 
					case [Slave || Slave <- Slaves, is_pid(Slave)] of
						[] ->
							fission_block:create(Path, Shift, master);
						[PID | _] ->
							PID ! {set_role, master},
							PID
					end,
					[Master] ++ [Slave || Slave <- Slaves, not is_pid(Slave)]
				});
			
			{set_slave, Path, Shift} ->
				{block, Master, Slaves} = table_get(Table, Path, Shift),
				Slave = fission_block:create(Path, Shift, slave),
				.rpc:sbcast(nodes(), fission, {table_change, Node, {set_slave, Path, Shift, Slave}}),
				table_set(Table, Path, Shift, {block, Master, [Slave | Slaves]})
		end,
		write_table({CurrentState#state.version + 1, NewTable}),
		CurrentState#state{
			version = CurrentState#state.version + 1,
			table = NewTable
		}
	end);
table_change(Node, Change, State) ->
	Table = State#state.table,
	NewTable = case Change of
		initialize ->
			table_set(Table, 0, 0, {block, Node, []});
		
		{split, Path, Shift} ->
			{block, Master, Slaves} = table_get(Table, Path, Shift),
			NewSlaves = .lists:map(fun
				(Slave) when is_pid(Slave) ->
					Slave ! split,
					receive
						{split, Path, Shift, Res} ->
							Res
					end;
				(Slave) ->
					{Slave, Slave, Slave, Slave}
			end, Slaves),
			table_set(Table, Path, Shift, {tree,
				{block, Master, [Slave || {Slave, _, _, _} <- NewSlaves]},
				{block, Master, [Slave || {_, Slave, _, _} <- NewSlaves]},
				{block, Master, [Slave || {_, _, Slave, _} <- NewSlaves]},
				{block, Master, [Slave || {_, _, _, Slave} <- NewSlaves]}
			});
		
		{set_master, Path, Shift} ->
			{block, Master, Slaves} = table_get(Table, Path, Shift),
			table_set(Table, Path, Shift, {block, Node, if
				Master == undefined ->
					[];
				is_pid(Master) ->
					Master ! {set_role, slave},
					[Master];
				true ->
					[Master]
			end ++ [Slave || Slave <- Slaves, Slave /= Node]});
		
		{set_slave, Path, Shift, PID} ->
			{block, Master, Slaves} = table_get(Table, Path, Shift),
			if
				is_pid(Master) ->
					Master ! {table_dump, PID};
				true ->
					true
			end,
			table_set(Table, Path, Shift, {block, Master, [Node | Slaves]})
	end,
	write_table({State#state.version + 1, NewTable}),
	State#state{
		version = State#state.version + 1,
		table = NewTable
	}.



cmd(Key, Command, From, State) ->
	Hash = .erlang:phash2(Key, 16#100000000),
	case table_get(State#state.table, Hash, 32) of
		{block, undefined, _Slaves} ->
			false;
		
		{block, Master, Slaves} when is_pid(Master) ->
			Replicate = case Command of
				get -> false;
				{set, _} -> true;
				{get_set, _} -> true;
				{set_nx, _} -> true;
				del -> true;
				{inc, _} -> true;
				{run, _} -> false;
				{spawn, _} -> false;
				{run_set, _} -> true;
				_ -> true % TODO this is wrong of course, but the replication needs to be rewritten anyway
			end,
			if
				Replicate ->
					.rpc:sbcast(Slaves, fission, {rep, Key, Command});
				true ->
					true
			end,
			%fission_util:log("~w at ~w from ~w", [Command, Key, From]),
			Master ! {cmd, Key, Command, From};
		
		{block, Master, _} ->
			{fission, Master} ! {cmd, Key, Command, From};
		
		_ ->
			false
	end,
	State.

rep(Key, Command, State) ->
	Hash = .erlang:phash2(Key, 16#100000000),
	{block, _Master, Slaves} = table_get(State#state.table, Hash, 32),
	fission_util:log("Replicating ~p at ~p", [Command, Key]),
	[Slave ! {cmd, Key, Command, false} || Slave <- Slaves, is_pid(Slave)],
	State.



worker(State) ->
	Version = State#state.version,
	Table = State#state.table,
	
	NewState = receive
	
		{cmd, Key, Command, From} ->
			cmd(Key, Command, From, State);
		
		% Network organization
		
		{nodeup, Node} ->
			fission_util:log("Node up: ~p", [Node]),
			{fission, Node} ! {version, Version, node()},
			State;
		{nodedown, Node} -> % TODO: negotiate about master change if any
			fission_util:log("Node down: ~p", [Node]),
			State;
		
		
		{version, NewVersion2, Node} ->
			if
				NewVersion2 > Version ->
					fission_util:log("Table upgrade from ~p: ~B -> ~B", [Node, Version, NewVersion2]),
					{fission, Node} ! {get_table, self()},
					receive % TODO move to separate function with timeout parameter (0 or infinity)
						{table, NewVersion, NewTable} ->
							fission_util:log("Table upgrade from ~p received: ~B -> ~B", [Node, Version, NewVersion]),
							% TODO: update tables
							write_table({NewVersion, NewTable}),
							State#state{
								version = NewVersion,
								table = NewTable
							}
					end;
				true ->
					State
			end;
		{get_table, From} ->
			From ! {table, Version,
				table_walk(Table, fun({block, Master, Slaves}, _Path, _Shift) ->
					{block,
						if
							is_pid(Master) -> node();
							true -> Master 
						end,
						.lists:map(fun
							(Slave) when is_pid(Slave) ->
								node();
							
							(Slave) ->
								Slave
						end, Slaves)
					}
				end)
			},
			State;
		
		
		
		% Data manipulation
		
		{table_change, Node, Change} ->
			table_change(Node, Change, State);
		
		{table_dump, Path, Shift, PID} ->
			{block, Master, _Slaves} = table_get(Table, Path, Shift),
			Master ! {table_dump, PID},
			State;
			
		{rewrite_aof, Path, Shift} ->
			{block, Master, _Slaves} = table_get(Table, Path, Shift),
			Master ! rewrite_aof,
			State;
			
		{rep, Key, Command} ->
			rep(Key, Command, State);
		
		
		
		% Misc
		
		stop ->
			.rpc:abcast(nodes(), fission, {nodedown, node()}),
			fission_util:log("Exiting"),
			exit(ok);
		
		debug ->
			fission_util:log("State: ~n~p", [State]),
			State;
		
		code_change ->
			fission_util:log("Reloading code"),
			State;
		
		Anything ->
			fission_util:log("Something received: ~p", [Anything]),
			State
	end,
	?MODULE:worker(NewState).

