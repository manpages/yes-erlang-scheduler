-module(odl_queue).

-include("../../include/task.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%-export([
%	get/2,
%	queue/3,
%	swap/3
%]).

first_seed(QID) -> 
	fission_syn:get_def({queue, QID, first}, 0).

size(QID) ->
	fission_syn:get_def({queue, QID, size}, 0).

insert_bottom(QID) ->
	fission_syn:inc({queue, QID, size}, 1),
	last_seed(QID)
.

caret(QID) ->
	fission_syn:get_def({queue, QID, caret},  0).

pull_caret(QID) ->
	fission_syn:inc({queue, QID, size}, 1),
	NewCaretPosition = fission_syn:inc_v({queue, QID, caret}, -1),
	case (first_seed(QID) > NewCaretPosition) of
		true -> fission_syn:set({queue, QID, first}, NewCaretPosition);
		_ -> ok
	end,
	NewCaretPosition
.

push_caret(QID) ->
	fission_syn:inc_v({queue, QID, caret}, 1).


last_seed(QID) -> 
	odl_queue:size(QID) - 1 + first_seed(QID).

range(QID) -> 
	range(QID, first_seed(QID), last_seed(QID)).
range(QID, First, Last) ->
	[odl_queue:get_entity(QID, X) || X <- lists:seq(First, Last)].

get(QID, EID) ->
	fission_syn:get({queue, QID, entity, EID}).

get_entity(QID, EID) ->
	case odl_queue:get(QID, EID) of
		{value, {task, TID}}  -> odl_task:get(TID);
		{value, {queue, QID}} -> range(QID)
	end.

top(QID) ->
	case fission_syn:get({queue, QID, entity, caret(QID)}) of
		{value, E} -> nil,E;
		_ -> false
	end
.

queue(TaskOrQueue, TargetQID, TopOrBottom) -> % {task, Task} | queue
	Entity = case TaskOrQueue of
		{task, Task} ->
			% first we actually store the Task data
			TID = fission_syn:inc_v(tasks),
			fission_syn:set({task, TID}, #taskT{
				task = Task
			}),
			?debugFmt("<<QUEUE>> Adding Task ~p under ID ~p~n", [Task, TID]),
			{task, TID};
		queue ->
			% or store new empty queue
			QID = fission_syn:inc_v(queues),
			{queue, QID}
	end,
	% then we link the newborn entity to the desired queue
	EID = case TopOrBottom of 
		top -> pull_caret(TargetQID);
		bottom -> insert_bottom(TargetQID)
	end,
	fission_syn:set({Entity, seed}, EID),
	?debugFmt("<<QUEUE>> Inserting ~p into queue with ID ~p at ~p~n", [Entity, TargetQID, EID]),
	?debugFmt("<<QUEUE>> First ~p; Last ~p; Size ~p", [first_seed(TargetQID), last_seed(TargetQID), odl_queue:size(TargetQID)]),
	fission_syn:set({queue, TargetQID, entity, EID}, Entity)
.

swap(Q, E1, E2) -> %EN = {task, TID}|{queue, QID}
	T1S = fission_syn:get_v({E2, seed}),
	T2S = fission_syn:get_v({E1, seed}),
	fission_syn:set({E1, seed}, T1S),
	fission_syn:set({E2, seed}, T2S),
	fission_zset:set({queue, Q, entity, T1S}, E1),
	fission_zset:set({queue, Q, entity, T2S}, E2)
.

do_not_run_this_function_ever() ->
	?debugFmt("~ts", [os:cmd("rm -rvf ./data/*")]),
	fission:initialize(node()),
	odl_queue:queue({task, 
		#os_taskT{
			cmd = "ls",
			params = "-la",
			args = "/home"
		}}
	,0, top),
	odl_queue:queue({task, 
		#os_taskT{
			cmd = "ls",
			params = "",
			args = "/usr/bin/"
		}}
	,0, bottom),
	odl_queue:queue(queue, 0, top),
	{ok, "ALL YOUR DATA ARE BELONG TO /dev/null"}.
