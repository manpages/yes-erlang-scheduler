-module(zset).

-compile(export_all).

new() ->
	{gb_trees:empty(), wtset:new()}.
	
set({T, S}, K, V) ->
	case gb_trees:lookup(K, T) of
		none ->
			{
				gb_trees:insert(K, V, T),
				wtset:insert(S, {V, K})
			};
		{value, OV} ->
			{
				gb_trees:update(K, V, T),
				wtset:replace(S, {OV, K}, {V, K})
			}
	end.
	
inc({T, S}, K, V) ->
	case gb_trees:lookup(K, T) of
		none ->
			{
				gb_trees:insert(K, V, T),
				wtset:insert(S, {V, K})
			};
		{value, OV} ->
			NV = OV + V,
			{
				gb_trees:update(K, NV, T),
				wtset:replace(S, {OV, K}, {NV, K})
			}
	end.

rank_left({T, S}, K) ->
	wtset:rank_left(S, {gb_trees:get(K, T), K}).
	
rank_right({T, S}, K) ->
	wtset:rank_right(S, {gb_trees:get(K, T), K}).
	
to_list_left({_, S}) ->
	wtset:to_list_left(S).

to_list_right({_, S}) ->
	wtset:to_list_right(S).
	
part_left({_, S}, O, N) ->
	wtset:part_left(S, O, N).
	
part_right({_, S}, O, N) ->
	wtset:part_right(S, O, N).
	
size({T, _}) ->
	gb_trees:size(T).