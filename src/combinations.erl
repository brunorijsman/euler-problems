-module(combinations).

-export([count/2,
         fold/4,
         all/2]).

-include_lib("eunit/include/eunit.hrl").

multiply_range(From, To) when To < From ->
    1;

multiply_range(From, To) ->
    From * multiply_range(From + 1, To).

div_range(Start, From, To) when To < From ->
    Start;

div_range(Start, From, To) ->
    div_range(Start div From, From + 1, To).

count(N, R) when R > N div 2 ->
    count(N, N - R);

count(N, R) ->
    div_range(multiply_range(N - R + 1, N), 1, R).

fold(Fun, Acc0, List, R) ->
    fold(Fun, Acc0, [], List, R).

fold(Fun, Acc0, Fixed, _RemainingList, 0) ->
    Fun(Fixed, Acc0);

fold(Fun, Acc0, Fixed, RemainingList, RemainingR) ->
    fold(Fun, Acc0, Fixed, RemainingList, RemainingR, length(RemainingList)).

fold(_Fun, Acc0, _Fixed, _RemainingList, _RemainingR, 0) ->
    Acc0;

fold(Fun, Acc0, Fixed, RemainingList, RemainingR, N) ->
    Move = lists:nth(N, RemainingList),
    NewRemainingList = lists:nthtail(N, RemainingList),
    NewRemainingR = RemainingR - 1,
    NewFixed = Fixed ++ [Move],
    Acc1 = fold(Fun, Acc0, NewFixed, NewRemainingList, NewRemainingR),
    fold(Fun, Acc1, Fixed, RemainingList, RemainingR, N-1). 

enlist(Combination, List) ->
    [Combination | List].

all(List, R) ->
    fold(fun enlist/2, [], List, R).

multiply_range_test() ->
    ?assertEqual(1, multiply_range(1, 1)),
    ?assertEqual(2, multiply_range(1, 2)),
    ?assertEqual(6, multiply_range(1, 3)),
    ?assertEqual(120, multiply_range(2, 5)),
    ?assertEqual(1, multiply_range(2, 1)).

div_range_test() ->
    ?assertEqual(2, div_range(10, 5, 5)),
    ?assertEqual(4, div_range(48, 3, 4)),
    ?assertEqual(4, div_range(240, 3, 5)).

count_test() ->
    ?assertEqual(5, count(5, 1)),
    ?assertEqual(1, count(5, 5)),
    ?assertEqual(10, count(5, 3)),
    ?assertEqual(10, count(5, 2)),
    ?assertEqual(18643560, count(40, 7)),
    ?assertEqual(658008, count(40, 35)).

all_test() ->
    ?assertEqual([[1],[2],[3],[4]], all([1,2,3,4], 1)),
    ?assertEqual([[1,2], [1,3], [1,4], [2,3], [2,4], [3,4]], all([1,2,3,4], 2)),
    ?assertEqual([[1,2,3], [1,2,4], [1,3,4], [2,3,4]], all([1,2,3,4], 3)),
    ?assertEqual([[1, 2, 3, 4]], all([1,2,3,4], 4)),
    ?assertEqual([], all([], 1)),
    ?assertEqual([], all([], 2)).
