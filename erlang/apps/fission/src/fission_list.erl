-module(fission_list).

-export([
	push/2,
	push_n/2,
	push_ff/2,
	
	sublist/2,
	sublist_a/2,
	sublist_v/2,
	sublist/3,
	sublist_a/3,
	sublist_v/3,
	sublist_ff/3
]).

push(Key, Element) ->
	fission_syn:run_set(Key, {?MODULE, push_ff, [Element]}).
	
push_n(Key, Element) ->
	fission_asyn:run_set_n(Key, {?MODULE, push_ff, [Element]}).
	
push_ff({value, List}, Element) ->
	{[Element | List], ok};

push_ff(false, Element) ->
	{[Element], ok}.
	

sublist(Key, Start, Len) ->
	fission_asyn:recv(sublist_a(Key, Start, Len)).
	
sublist_a(Key, Start, Len) ->
	fission_asyn:spawn(Key, {?MODULE, sublist_ff, [Start, Len]}).

sublist_v(Key, Start, Len) ->
	{_, Value} = sublist(Key, Start, Len),
	Value.

sublist_ff(false, _, _) ->
	[];
sublist_ff({value, List}, Start, Len) ->
	lists:sublist(List, Start, Len).
	
sublist(Key, Len) ->
	sublist(Key, 1, Len).

sublist_a(Key, Len) ->
	sublist_a(Key, 1, Len).

sublist_v(Key, Len) ->
	sublist_v(Key, 1, Len).