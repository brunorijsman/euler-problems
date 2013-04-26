-module(lists2).

-export([rotate_left/1,
         rotate_right/1,
         all_rotations/1,
         uniq/1,
         find_solution/2]).

-include_lib("eunit/include/eunit.hrl").

rotate_left([]) ->
    [];

rotate_left(List) ->
    [First | Rest] = List,
    Rest ++ [First].

rotate_right([]) ->
    [];

rotate_right(List) ->
    Last = lists:last(List),
    Rest = lists:sublist(List, length(List) - 1),
    [Last | Rest].

all_rotations(List) ->
    all_rotations(List, List, []).

all_rotations(OriginalList, List, Rotations) ->
    NewRotations = [List | Rotations],
    NextRotation = rotate_left(List),
    case NextRotation of
        OriginalList -> NewRotations;
        _            -> all_rotations(OriginalList, NextRotation, NewRotations)
    end.

%% Given a sorted list, remove the dupplicates
%%
uniq(List) ->
    lists:reverse(uniq2(List, [])).

uniq2([], U) ->
    U;

uniq2([E], U) ->
    [E | U];

uniq2([E1, E1 | Rest], U) ->
    uniq2([E1 | Rest], U);

uniq2([E | Rest], U) ->
    uniq2(Rest, [E | U]).

%% Apply function Fun to each element in list List, starting at the first element.
%%
%% Fun(Element) must return either false or {true, Solution}.
%%
%% find_solution returns:
%%
%% 1. {true, Solution} for the first value of Element for which Fun(Element)
%%    returns {true, Solution}, or
%%x
%% 2. false is Fun(Element) does not return {true, Solution} for any element.
%%
find_solution(_Fun, []) ->
    false;

find_solution(Fun, [Element | Rest]) ->
    case Fun(Element) of
        {true, Solution} -> {true, Solution};
        false            -> find_solution(Fun, Rest)
    end.

rotate_left_test() ->
    ?assertEqual([], rotate_left([])),
    ?assertEqual([1], rotate_left([1])),
    ?assertEqual([2, 1], rotate_left([1, 2])),
    ?assertEqual([2, 3, 1], rotate_left([1, 2, 3])),
    ?assertEqual([1, 1, 1, 1], rotate_left([1, 1, 1, 1])).

rotate_right_test() ->
    ?assertEqual([], rotate_right([])),
    ?assertEqual([1], rotate_right([1])),
    ?assertEqual([2, 1], rotate_right([1, 2])),
    ?assertEqual([3, 1, 2], rotate_right([1, 2, 3])),
    ?assertEqual([1, 1, 1, 1], rotate_right([1, 1, 1, 1])).

all_rotations_test() ->
    ?assertEqual([[]], all_rotations([])),
    ?assertEqual([[1]], all_rotations([1])),
    ?assertEqual([[1, 2], [2, 1]], lists:sort(all_rotations([1, 2]))),
    ?assertEqual([[1, 2, 3], [2, 3, 1], [3, 1, 2]], lists:sort(all_rotations([1, 2, 3]))),
    ?assertEqual([[1, 1]], lists:sort(all_rotations([1, 1]))),
    ?assertEqual([[1, 2, 1, 2], [2, 1, 2, 1]], lists:sort(all_rotations([1, 2, 1, 2]))).

uniq_test() ->
    ?assertEqual([], uniq([])),
    ?assertEqual([1], uniq([1])),
    ?assertEqual([1, 2], uniq([1, 2])),
    ?assertEqual([1, 2, 3], uniq([1, 2, 3])),
    ?assertEqual([1, 2, 3, 4], uniq([1, 2, 3, 4])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 2, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 1, 2, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 2, 2, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 1, 2, 3, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 2, 2, 2, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 2, 2, 2, 3, 3, 3, 4, 5])),
    ?assertEqual([1, 2, 3, 4, 5], uniq([1, 2, 3, 4, 5, 5, 5])).
    
triple_if_even(N) ->
    case N rem 2 of
        0 -> {true, 3 * N}; 
        _ -> false
    end.

find_solution_test() ->
    ?assertEqual({true, 6}, find_solution(fun triple_if_even/1, [1, 3, 2, 4, 5])),
    ?assertEqual({true, 6}, find_solution(fun triple_if_even/1, [2, 3, 4, 6, 7])),
    ?assertEqual({true, 12}, find_solution(fun triple_if_even/1, [1, 3, 4])),
    ?assertEqual({true, 18}, find_solution(fun triple_if_even/1, [6])),
    ?assertEqual(false, find_solution(fun triple_if_even/1, [])),
    ?assertEqual(false, find_solution(fun triple_if_even/1, [1, 3, 5])).
    
                     
