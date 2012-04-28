-module(fission_tuple).

-export([
	set/3,
	set_n/3,
	set_ff/3,
	
	get/2,
	get_asyn/2,
	get_ff/2,
	get_v/2
]).


set(Key, Index, Value) ->
	fission_syn:run_set(Key, {?MODULE, set_ff, [Index, Value]}).
	
set_n(Key, Index, Value) ->
	fission_asyn:run_set_n(Key, {?MODULE, set_ff, [Index, Value]}).
	
set_ff({value, Tuple}, Index, Value) ->
	{setelement(Index, Tuple, Value), ok}.
	
	
get(Key, Index) ->
	fission_syn:run(Key, {?MODULE, get_ff, [Index]}).
	
get_asyn(Key, Index) ->
	fission_asyn:run(Key, {?MODULE, get_ff, [Index]}).
	
get_ff({value, Tuple}, Index) ->
	element(Index, Tuple);
	
get_ff(false, _) ->
	undefined.
	
get_v(Key, Index) ->
	{result, Value} = ?MODULE:get(Key, Index),
	Value.