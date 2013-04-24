-module(euler67).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

get_test_triangle() ->
    [[3],
     [7, 4],
     [2, 4, 6],
     [8, 5, 9, 3]].

read_triangle(FileName, NrRows) ->
    {ok, Fd} = file:open(FileName, [read]),
    Triangle = read_rows(Fd, 1, NrRows, []),
    ok = file:close(Fd),
    lists:reverse(Triangle).

read_rows(_Fd, RowNr, NrRows, Triangle) when RowNr > NrRows ->
    Triangle;

read_rows(Fd, RowNr, NrRows, Triangle) ->
    Row = read_one_row(Fd, RowNr, []),
    read_rows(Fd, RowNr + 1, NrRows, [Row | Triangle]).

read_one_row(_Fd, MoreNrs, RowSoFar) when MoreNrs == 0 ->
    lists:reverse(RowSoFar);

read_one_row(Fd, MoreNrs, RowSoFar) ->
    {ok, [Nr]} = io:fread(Fd, "", "~d"),
    read_one_row(Fd, MoreNrs - 1, [Nr | RowSoFar]).

solve() ->
    Triangle = read_triangle("../input/euler67.txt", 100),
    solve(Triangle).

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
