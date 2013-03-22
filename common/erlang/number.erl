-module(number).

-export([to_list/1,
         from_list/1,
	 all_rotations/1,
         truncate_left/1,
         all_truncations_left/1,
         truncate_right/1,
         all_truncations_right/1]).

-include_lib("eunit/include/eunit.hrl").

to_rev_list(Nr) when Nr == 0 ->
  [];

to_rev_list(Nr) ->
  [ Nr rem 10 | to_rev_list(Nr div 10) ].

to_list(0) ->
  [0];

to_list(Nr) ->
  lists:reverse(to_rev_list(Nr)).

from_rev_list([]) ->
  0;

from_rev_list([Digit | Rest]) ->
  Digit + 10 * from_rev_list(Rest).

from_list(List) ->
  from_rev_list(lists:reverse(List)).

all_rotations(Nr) ->
  lists:map(fun from_list/1, lists2:all_rotations(to_list(Nr))).

truncate_left(Nr) ->
  [_Digit | Rest] = to_list(Nr),
  from_list(Rest).

all_truncations_left(Nr) ->
  all_truncations_left(truncate_left(Nr), [Nr]).

all_truncations_left(0, Truncations) ->
  Truncations;

all_truncations_left(Nr, Truncations) ->
  all_truncations_left(truncate_left(Nr), [Nr | Truncations]).

truncate_right(Nr) ->
  [_Digit | Rest] = lists:reverse(to_list(Nr)),
  from_list(lists:reverse(Rest)).

all_truncations_right(Nr) ->
  all_truncations_right(truncate_right(Nr), [Nr]).

all_truncations_right(0, Truncations) ->
  Truncations;

all_truncations_right(Nr, Truncations) ->
  all_truncations_right(truncate_right(Nr), [Nr | Truncations]).

is_square(N) ->
  Root = trunc(math:sqrt(N)),
  Root * Root == N.

% Is N a triangle number?
% N is a triangle number if N = K*(K+1)/2 for some integer K.
% Given an N and solving for K you get K = (sqrt(8N+1)-1)/2.
% N is a triangle number if K is integer which is true if 8N+1 is square.
%
is_triangle(N) ->
  is_square(8*N+1).

to_list_test() ->
  ?assertEqual([0], to_list(0)),
  ?assertEqual([1], to_list(1)),
  ?assertEqual([9], to_list(9)),
  ?assertEqual([1, 0], to_list(10)),
  ?assertEqual([1, 1], to_list(11)),
  ?assertEqual([1, 9], to_list(19)),
  ?assertEqual([5, 5, 4, 4, 3, 3, 2, 2, 1, 1], to_list(5544332211)).

from_list_test() ->
  ?assertEqual(0, from_list([])),
  ?assertEqual(0, from_list([0])),
  ?assertEqual(1, from_list([1])),
  ?assertEqual(9, from_list([9])),
  ?assertEqual(10, from_list([1, 0])),
  ?assertEqual(11, from_list([1, 1])),
  ?assertEqual(19, from_list([1, 9])),
  ?assertEqual(5544332211, from_list([5, 5, 4, 4, 3, 3, 2, 2, 1, 1])).

all_rotations_test() ->
  ?assertEqual([0], all_rotations(0)),
  ?assertEqual([1], all_rotations(1)),
  ?assertEqual([1, 10], lists:sort(all_rotations(10))),
  ?assertEqual([123, 231, 312], lists:sort(all_rotations(123))),
  ?assertEqual([8989, 9898], lists:sort(all_rotations(8989))).

truncate_left_test() ->
  ?assertEqual(2345, truncate_left(12345)),
  ?assertEqual(1, truncate_left(11)),
  ?assertEqual(3, truncate_left(303)),
  ?assertEqual(0, truncate_left(3)),
  ?assertEqual(0, truncate_left(0)).

all_truncations_left_test() ->
  ?assertEqual([5, 45, 345, 2345, 12345], lists:sort(all_truncations_left(12345))),
  ?assertEqual([1, 701, 80701], lists:sort(all_truncations_left(80701))),
  ?assertEqual([3], lists:sort(all_truncations_left(3))),
  ?assertEqual([0], lists:sort(all_truncations_left(0))).

truncate_right_test() ->
  ?assertEqual(1234, truncate_right(12345)),
  ?assertEqual(1, truncate_right(11)),
  ?assertEqual(30, truncate_right(303)),
  ?assertEqual(30, truncate_right(300)),
  ?assertEqual(0, truncate_right(3)),
  ?assertEqual(0, truncate_right(0)).

all_truncations_right_test() ->
  ?assertEqual([1, 12, 123, 1234, 12345], lists:sort(all_truncations_right(12345))),
  ?assertEqual([8, 80, 807, 8070, 80701], lists:sort(all_truncations_right(80701))),
  ?assertEqual([3], lists:sort(all_truncations_right(3))),
  ?assertEqual([0], lists:sort(all_truncations_right(0))).

is_square_test() ->
  ?assert(is_square(1)),
  ?assert(is_square(4)),
  ?assert(is_square(9)),
  ?assert(is_square(16)),
  ?assertNot(is_square(2)),
  ?assertNot(is_square(10)).

is_triangle_test() ->
  ?assert(is_triangle(1)),
  ?assert(is_triangle(3)),
  ?assert(is_triangle(6)),
  ?assert(is_triangle(10)),
  ?assert(is_triangle(55)),
  ?assertNot(is_triangle(2)),
  ?assertNot(is_triangle(16)),
  ?assertNot(is_triangle(56)).
