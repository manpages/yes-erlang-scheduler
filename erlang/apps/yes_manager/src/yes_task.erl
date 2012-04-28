-module(odl_task).

-compile({parse_transform, exprecs}).

-include("../../include/task.hrl").
-include_lib("eunit/include/eunit.hrl").

-export_records([taskT, os_taskT, erl_taskT, ext_taskT]).

-export([
	get/1,
	get_json/1
]).

%get_first(QID) -> 
%	case fission_zset:part_left({queue,QID}, 0, 1) of 
%		{result, [{Seed, {EntityT, EID}]} -> {Seed, TaskID};
%		_ -> false
%	end
%.
%get_top_task() -> 
%	case get_top() of
%		{_,ID} -> sch_task:get(ID);
%		false -> false
%	end
%.
get(TID) ->
	{Desc, Task} = case fission_syn:get_def({task, TID}, false) of
		false -> {false, undefined};
		Data -> {{description,
			{[{id, TID},
			{type, erlang:element(1, Data#taskT.task)},
			{status, case Data#taskT.started of
				undefined -> pending;
				X -> case Data#taskT.finished of
					undefined -> {solving, X};
					S -> {done, S}
				end
			end},
			{result, Data#taskT.result}]}
		}, Data}
	end,
	Contents = case Task#taskT.task of
		undefined -> false;
		TaskDef -> %{data, ?MODULE:'#info-taskT'(fields)}
			{data, {[{X, ?MODULE:(do("#get-", TaskDef))(X, TaskDef)} || X <- ?MODULE:(do("#info-", TaskDef))(fields)]}}
	end,
	{[Desc, Contents]}.

do(Action, Record) ->
	list_to_existing_atom(Action ++ atom_to_list(erlang:element(1, Record))).

get_json(TID) ->
	Bin = jiffy:encode(odl_task:get(TID)),
	?debugFmt("~ts", [Bin]),
	Bin.
