-module(euler67).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

get_test_triangle() ->
    [[3],
     [7, 4],
     [2, 4, 6],
     [8, 5, 9, 3]].

solve() ->
    solve(get_test_triangle()).

solve([LastRow]) ->
    lists:max(LastRow);

solve([Row1, Row2 | Rest]) ->
    Row1a = [0] ++ Row1 ++ [0], 
    MergedRow = merge_rows(Row1a, Row2, []),
    solve([MergedRow | Rest]).
    
merge_rows(_Row1, _Row2 = [], Merged) ->
    lists:reverse(Merged);

merge_rows(_Row1 = [E11, E12 | Rest1], _Row2 = [E2 | Rest2], Merged) ->
    Max = max(E11 + E2, E12 + E2),
    merge_rows([E12 | Rest1], Rest2, [Max | Merged]).

solve_test() ->
    ?assertEqual(23, solve(get_test_triangle())).
