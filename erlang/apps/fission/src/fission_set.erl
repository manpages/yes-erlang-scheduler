-module(fission_set).

-export([
	add/2,
	add_n/2,
	add_ff/2,
	
	del/2,
	del_n/2,
	del_ff/2,
	
	size/1,
	size_asyn/1,
	size_ff/1,
	size_v/1,
	
	is_member/2,
	is_member_asyn/2,
	is_member_ff/2,
	is_member_v/2,
	
	to_list/1,
	to_list_asyn/1,
	to_list_ff/1,
	to_list_v/1
]).

add(Key, Element) ->
	fission_syn:run_set(Key, {?MODULE, add_ff, [Element]}).
	
add_n(Key, Element) ->
	fission_asyn:run_set_n(Key, {?MODULE, add_ff, [Element]}).
	
add_ff({value, Set}, Element) ->
	{gb_sets:add(Element, Set), ok};

add_ff(false, Element) ->
	{gb_sets:singleton(Element), ok}.
	

del(Key, Element) ->
	fission_syn:run_set(Key, {?MODULE, del_ff, [Element]}).
	
del_n(Key, Element) ->
	fission_asyn:run_set_n(Key, {?MODULE, del_ff, [Element]}).
	
del_ff({value, Set}, Element) ->
	{gb_sets:del_element(Element, Set), ok};

del_ff(false, _) ->
	{gb_sets:empty(), ok}.
	
	
size(Key) ->
	fission_syn:run(Key, {?MODULE, size_ff}).
	
size_asyn(Key) ->
	fission_asyn:run(Key, {?MODULE, size_ff}).
	
size_ff({value, Set}) ->
	gb_sets:size(Set);
	
size_ff(false) ->
	0.
	
size_v(Key) ->
	{result, Value} = ?MODULE:size(Key),
	Value.
	
	
is_member(Key, Element) ->
	fission_syn:run(Key, {?MODULE, is_member_ff, [Element]}).
	
is_member_asyn(Key, Element) ->
	fission_asyn:run(Key, {?MODULE, is_member_ff, [Element]}).
	
is_member_ff({value, Set}, Element) ->
	gb_sets:is_member(Element, Set);
	
is_member_ff(false, _) ->
	false.
	
is_member_v(Key, Element) ->
	{result, Value} = ?MODULE:is_member(Key, Element),
	Value.
	
	
to_list(Key) ->
	fission_syn:run(Key, {?MODULE, to_list_ff}).
	
to_list_asyn(Key) ->
	fission_asyn:run(Key, {?MODULE, to_list_ff}).
	
to_list_ff({value, Set}) ->
	gb_sets:to_list(Set);
	
to_list_ff(false) ->
	[].
	
to_list_v(Key) ->
	{result, Value} = ?MODULE:to_list(Key),
	Value.