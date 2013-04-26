-module(permutations).

-export([fold/3,
         all/1,
         is_permutation_of/2]).

-include_lib("eunit/include/eunit.hrl").

fold(Fun, Acc0, List) ->
    fold(Fun, Acc0, [], List).

fold(Fun, Acc0, Fixed, []) ->
    Fun(Fixed, Acc0);

fold(Fun, Acc0, Fixed, Remaining) ->
    fold(Fun, Acc0, Fixed, Remaining, length(Remaining)).

fold(_Fun, Acc0, _Fixed, _Remaining, 0) ->
    Acc0;

fold(Fun, Acc0, Fixed, Remaining, N) ->
    Move = lists:nth(N, Remaining),
    NewRemaining = lists:sublist(Remaining, N-1) ++ lists:nthtail(N, Remaining),
    NewFixed = Fixed ++ [Move],
    Acc1 = fold(Fun, Acc0, NewFixed, NewRemaining),
    fold(Fun, Acc1, Fixed, Remaining, N-1). 

enlist(Permutation, List) ->
    [Permutation | List].

all(List) ->
    fold(fun enlist/2, [], List).

is_permutation_of(List1, List2) ->
    lists:sort(List1) == lists:sort(List2).

is_permutation_of_test() ->
    ?assert(is_permutation_of([1, 2, 3, 4], [1, 2, 3, 4])),
    ?assert(is_permutation_of([1, 2, 3, 4], [4, 2, 1, 3])),
    ?assert(is_permutation_of([1], [1])),
    ?assertNot(is_permutation_of([1, 2, 3, 4], [4, 2, 5, 3])),
    ?assertNot(is_permutation_of([1, 2, 3, 4], [4, 2, 3])).

all_test() ->
    ?assertEqual([[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]], all([1,2,3])).
