-module(lists2).

-export([rotate_left/1,
         rotate_right/1,
         all_rotations/1]).

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