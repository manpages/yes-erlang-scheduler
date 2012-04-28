-record(taskT, {
	task,
	result = undefined,
	started = undefined,
	finished = undefined
}).

-record(os_taskT, {
	cmd,
	params,
	args
}).

-record(erl_taskT, {
	module,
	function,
	args
}).

-record(ext_taskT, {
	data
}).
