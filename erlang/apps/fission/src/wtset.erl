-module(wtset).

-compile(export_all).


new() ->
	nil.
	
insert(nil, V) ->
	{V, 1, nil, nil};
insert({E, C, L, R}, V) when V > E ->
	balance({E, C + 1, L, insert(R, V)});
insert({E, C, L, R}, V) when V < E ->
	balance({E, C + 1, insert(L, V), R}).
	
delete({V, 1, nil, nil}, V) ->
	nil;
delete({V, _, L, nil}, V) ->
	L;
delete({V, _, nil, R}, V) ->
	R;
delete({E, C, L, R}, V) when V > E ->
	balance({E, C - 1, L, delete(R, V)});
delete({E, C, L, R}, V) when V < E ->
	balance({E, C - 1, delete(L, V), R});
delete({V, C, L, R}, V) ->
	{A, NR} = take_smallest(R),
	balance({A, C - 1, L, NR}).
	
replace(T, OV, NV) ->
	insert(delete(T, OV), NV). % TODO something cool


balance(T = {_, _, L, R}) ->
	WL = weight(L),
	WR = weight(R),
	OK = (WL + WR < 2)
		orelse (WL < 2 andalso WR < 2)
		orelse (WL * 5 > WR andalso WR * 5 > WL),
	case OK of
		true ->
			T;
		false when (WL < WR) ->
			{_, _, SL, SR} = R,
			case weight(SR) > weight(SL) of
				true ->
					rotate_l1(T);
				false ->
					rotate_l2(T)
			end;
		false when (WL > WR) ->
			{_, _, SL, SR} = L,
			case weight(SR) < weight(SL) of
				true ->
					rotate_r1(T);
				false ->
					rotate_r2(T)
			end
	end.
	
weight(nil) -> 0;
weight({_, C, _, _}) -> C.


rotate_l1({A, _, X, {B, _, Y, Z}}) ->
	WA = weight(X) + weight(Y) + 1,
	{B, WA + weight(Z) + 1, {A, WA, X, Y}, Z}.
	
rotate_r1({B, _, {A, _, X, Y}, Z}) ->
	WB = weight(Y) + weight(Z) + 1,
	{A, WB + weight(X) + 1, X, {B, WB, Y, Z}}.
	
rotate_l2({A, _, X, {C, _, {B, _, Y1, Y2}, Z}}) ->
	WA = weight(X) + weight(Y1) + 1,
	WC = weight(Y2) + weight(Z) + 1,
	{B, WA + WC + 1, {A, WA, X, Y1}, {C, WC, Y2, Z}}.
	
rotate_r2({A, _, {C, _, Z, {B, _, Y2, Y1}}, X}) ->
	WA = weight(X) + weight(Y1) + 1,
	WC = weight(Y2) + weight(Z) + 1,
	{B, WA + WC + 1, {C, WC, Z, Y2}, {A, WA, Y1, X}}.


take_smallest({V, 1, nil, nil}) ->
	{V, nil};
take_smallest({V, _, nil, R}) ->
	{V, R};
take_smallest({E, C, L, R}) ->
	{V, NL} = take_smallest(L),
	{V, {E, C - 1, NL, R}}.


rank_left(T, V) ->
	rank_left(T, V, 0).
rank_left({E, _, L, R}, V, C) when V > E ->
	rank_left(R, V, C + weight(L) + 1);
rank_left({E, _, L, _}, V, C) when V < E ->
	rank_left(L, V, C);
rank_left({V, _, L, _}, V, C) ->
	C + weight(L).

rank_right(T, V) ->
	rank_right(T, V, 0).
rank_right({E, _, _, R}, V, C) when V > E ->
	rank_right(R, V, C);
rank_right({E, _, L, R}, V, C) when V < E ->
	rank_right(L, V, C + weight(R) + 1);
rank_right({V, _, _, R}, V, C) ->
	C + weight(R).


from_list(List) ->
	from_list(List, new()).
from_list([], Acc) ->
	Acc;
from_list([H|T], Acc) ->
	from_list(T, insert(Acc, H)).


to_list_left(T) ->
	to_list_left(T, []).
to_list_left(nil, Acc) ->
	Acc;
to_list_left({E, _, L, R}, Acc) ->
	to_list_left(L, [E|to_list_left(R, Acc)]).
	
to_list_right(T) ->
	to_list_right(T, []).
to_list_right(nil, Acc) ->
	Acc;
to_list_right({E, _, L, R}, Acc) ->
	to_list_right(R, [E|to_list_right(L, Acc)]).
	
	
part_left(T, S, N) ->
	lists:reverse(part_left(T, S, N, [])).
part_left(nil, _, _, Acc) ->
	Acc;
part_left(_, _, 0, Acc) ->
	Acc;
part_left({E, _, L, R}, S, N, Acc) ->
	WL = weight(L),
	if
		WL > S ->
			if
				WL >= S + N ->
					part_left(L, S, N, Acc);
				true ->
					PL = case S of
						0 -> to_list_right(L, Acc);
						_ -> part_left(L, S, N, Acc)
					end,
					part_left(R, 0, S + N - WL - 1, [E | PL])
			end;
		WL == S ->
			part_left(R, 0, N - 1, [E | Acc]);
		true ->
			part_left(R, S - WL - 1, N, Acc)
	end.
	
	
part_right(T, S, N) ->
	lists:reverse(part_right(T, S, N, [])).
part_right(nil, _, _, Acc) ->
	Acc;
part_right(_, _, 0, Acc) ->
	Acc;
part_right({E, _, L, R}, S, N, Acc) ->
	WR = weight(R),
	if
		WR > S ->
			if
				WR >= S + N ->
					part_right(R, S, N, Acc);
				true ->
					PR = case S of
						0 -> to_list_left(R, Acc);
						_ -> part_right(R, S, N, Acc)
					end,
					part_right(L, 0, S + N - WR - 1, [E | PR])
			end;
		WR == S ->
			part_right(L, 0, N - 1, [E | Acc]);
		true ->
			part_right(L, S - WR - 1, N, Acc)
	end.