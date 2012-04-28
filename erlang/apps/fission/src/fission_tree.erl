-module(fission_tree).

-export([
	insert/3,
	insert_n/3,
	insert_ff/3,
	
	del/2,
	del_n/2,
	del_ff/2,
	
	size/1,
	size_asyn/1,
	size_ff/1,
	size_v/1,
	
	is_defined/2,
	is_defined_asyn/2,
	is_defined_ff/2,
	is_defined_v/2
]).

insert(Key, TKey, Value) ->
	fission_syn:run_set(Key, {?MODULE, insert_ff, [TKey, Value]}).
	
insert_n(Key, TKey, Value) ->
	fission_asyn:run_set_n(Key, {?MODULE, insert_ff, [TKey, Value]}).
	
insert_ff({value, Set}, TKey, Value) ->
	{gb_trees:insert(TKey, Value, Set), ok};

insert_ff(false, TKey, Value) ->
	{gb_trees:insert(TKey, Value, gb_trees:empty()), ok}.
	

del(Key, TKey) ->
	fission_syn:run_set(Key, {?MODULE, del_ff, [TKey]}).
	
del_n(Key, TKey) ->
	fission_asyn:run_set_n(Key, {?MODULE, del_ff, [TKey]}).
	
del_ff({value, Set}, TKey) ->
	{gb_trees:delete_any(TKey, Set), ok};

del_ff(false, _) ->
	{gb_trees:empty(), ok}.
	
	
size(Key) ->
	fission_syn:run(Key, {?MODULE, size_ff}).
	
size_asyn(Key) ->
	fission_asyn:run(Key, {?MODULE, size_ff}).
	
size_ff({value, Set}) ->
	gb_trees:size(Set);
	
size_ff(false) ->
	0.
	
size_v(Key) ->
	{result, Value} = ?MODULE:size(Key),
	Value.
	
	
is_defined(Key, TKey) ->
	fission_syn:run(Key, {?MODULE, is_defined_ff, [TKey]}).
	
is_defined_asyn(Key, TKey) ->
	fission_asyn:run(Key, {?MODULE, is_defined_ff, [TKey]}).
	
is_defined_ff({value, Set}, TKey) ->
	gb_trees:is_defined(TKey, Set);
	
is_defined_ff(false, _) ->
	false.
	
is_defined_v(Key, TKey) ->
	{result, Value} = ?MODULE:is_defined(Key, TKey),
	Value.
	