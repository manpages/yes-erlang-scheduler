-module(fission_zset).

-export([
	set/3,
	set_n/3,
	set_ff/3,
	
	inc/3,
	inc_n/3,
	inc_ff/3,
	
	part_left/3,
	part_left_v/3,
	part_left_a/3,
	part_left_ff/3,
	
	part_right/3,
	part_right_v/3,
	part_right_a/3,
	part_right_ff/3,
	
	size/1,
	size_v/1,
	size_a/1,
	size_ff/1
]).


set(Key, TKey, Value) ->
	fission_syn:run_set(Key, {?MODULE, set_ff, [TKey, Value]}).
	
set_n(Key, TKey, Value) ->
	fission_asyn:run_set_n(Key, {?MODULE, set_ff, [TKey, Value]}).
	
set_ff({value, Set}, TKey, Value) ->
	{zset:set(Set, TKey, Value), ok};

set_ff(false, TKey, Value) ->
	{zset:set(zset:new(), TKey, Value), ok}.
	

inc(Key, TKey, Value) ->
	fission_syn:run_set(Key, {?MODULE, inc_ff, [TKey, Value]}).
	
inc_n(Key, TKey, Value) ->
	fission_asyn:run_set_n(Key, {?MODULE, inc_ff, [TKey, Value]}).
	
inc_ff({value, Set}, TKey, Value) ->
	{zset:inc(Set, TKey, Value), ok};

inc_ff(false, TKey, Value) ->
	{zset:set(zset:new(), TKey, Value), ok}.
	
	
part_left(Key, Offset, Limit) ->
	fission_syn:run(Key, {?MODULE, part_left_ff, [Offset, Limit]}).
	
part_left_v(Key, Offset, Limit) ->
	fission_syn:run_v(Key, {?MODULE, part_left_ff, [Offset, Limit]}).

part_left_a(Key, Offset, Limit) ->
	fission_asyn:run(Key, {?MODULE, part_left_ff, [Offset, Limit]}).
	
part_left_ff({value, Set}, Offset, Limit) ->
	zset:part_left(Set, Offset, Limit);

part_left_ff(false, _, _) ->
	[].
	

part_right(Key, Offset, Limit) ->
	fission_syn:run(Key, {?MODULE, part_right_ff, [Offset, Limit]}).
	
part_right_v(Key, Offset, Limit) ->
	fission_syn:run_v(Key, {?MODULE, part_right_ff, [Offset, Limit]}).

part_right_a(Key, Offset, Limit) ->
	fission_asyn:run(Key, {?MODULE, part_right_ff, [Offset, Limit]}).
	
part_right_ff({value, Set}, Offset, Limit) ->
	zset:part_right(Set, Offset, Limit);

part_right_ff(false, _, _) ->
	[].
	
	
size(Key) ->
	fission_syn:run(Key, {?MODULE, size_ff}).
	
size_v(Key) ->
	fission_syn:run_v(Key, {?MODULE, size_ff}).

size_a(Key) ->
	fission_asyn:run(Key, {?MODULE, size_ff}).
	
size_ff({value, Set}) ->
	zset:size(Set);

size_ff(false) ->
	0.