-record(nodeT, {
	node,
	status,
	since,
	task = undefined
}).

-record(node_mon, {
	dict,
	idle
}).
