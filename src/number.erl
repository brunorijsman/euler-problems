-module(number).

-export([to_list/1,
         from_list/1,
         all_rotations/1,
         truncate_left/1,
         all_truncations_left/1,
         truncate_right/1,
         all_truncations_right/1,
         is_square/1,
         is_triangle/1,
         is_pentagonal/1,
         is_hexagonal/1,
         digit_sum/1,
         int_pow/2,
         same_digits/2,
         all_permutations/1,
         nr_digits/1,
         concat/2]).

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
                                                % N is a triangle number if K is integer which is true if 8N+1 is square
                                                % andalso sqrt(8N+1)-1 is a multiple of 2
                                                %
is_triangle(N) ->
    is_square(8*N+1) andalso (round(math:sqrt(8*N+1))+1) rem 2 == 0.

is_pentagonal(N) ->
    is_square(24*N+1) andalso (round(math:sqrt(24*N+1))+1) rem 6 == 0.

is_hexagonal(N) ->
    is_square(8*N+1) andalso (round(math:sqrt(8*N+1))+1) rem 4 == 0.

digit_sum(0) ->
    0;

digit_sum(N) ->
    (N rem 10) + digit_sum(N div 10).

int_pow(_A, B) when B == 0 ->
    1;

int_pow(A, B) ->
    A * int_pow(A, B-1).

same_digits(N1, N2) ->
    permutations:is_permutation_of(to_list(N1), to_list(N2)).

non_zero([0|_]) -> 
    false;

non_zero(_) -> 
    true.

nr_digits(N) ->
    if
        N < 0  -> nr_digits_recurse(-N);
        N == 0 -> 1;
        N > 0  -> nr_digits_recurse(N)
    end.

nr_digits_recurse(N) when N == 0 ->
    0;

nr_digits_recurse(N) ->
    1 + nr_digits_recurse(N div 10).

concat(N1, N2) ->
    from_list(to_list(N1) ++ to_list(N2)).

all_permutations(N) ->
    L1 = permutations:all(number:to_list(N)),
    L2 = lists:filter(fun non_zero/1, L1),
    L3 = lists:map(fun number:from_list/1, L2),
    lists:usort(L3). 

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
    ?assertNot(is_triangle(2)),
    ?assert(is_triangle(3)),
    ?assertNot(is_triangle(4)),
    ?assertNot(is_triangle(5)),
    ?assert(is_triangle(6)),
    ?assertNot(is_triangle(7)),
    ?assertNot(is_triangle(8)),
    ?assertNot(is_triangle(9)),
    ?assert(is_triangle(10)),
    ?assertNot(is_triangle(16)),
    ?assert(is_triangle(55)),
    ?assertNot(is_triangle(56)).

is_pentagonal_test() ->
    ?assert(is_pentagonal(1)),
    ?assertNot(is_pentagonal(2)),
    ?assertNot(is_pentagonal(3)),
    ?assertNot(is_pentagonal(4)),
    ?assert(is_pentagonal(5)),
    ?assertNot(is_pentagonal(6)),
    ?assertNot(is_pentagonal(7)),
    ?assertNot(is_pentagonal(8)),
    ?assertNot(is_pentagonal(11)),
    ?assert(is_pentagonal(12)),
    ?assertNot(is_pentagonal(13)),
    ?assert(is_pentagonal(22)),
    ?assert(is_pentagonal(35)).

is_hexagonal_test() ->
    ?assert(is_hexagonal(1)),
    ?assertNot(is_hexagonal(2)),
    ?assertNot(is_hexagonal(3)),
    ?assertNot(is_hexagonal(4)),
    ?assertNot(is_hexagonal(5)),
    ?assert(is_hexagonal(6)),
    ?assertNot(is_hexagonal(7)),
    ?assertNot(is_hexagonal(8)),
    ?assertNot(is_hexagonal(9)),
    ?assertNot(is_hexagonal(14)),
    ?assert(is_hexagonal(15)),
    ?assertNot(is_hexagonal(16)),
    ?assert(is_hexagonal(28)),
    ?assert(is_hexagonal(45)).

digit_sum_test() ->
    ?assertEqual(0, digit_sum(0)),
    ?assertEqual(1, digit_sum(1)),
    ?assertEqual(3, digit_sum(12)),
    ?assertEqual(12, digit_sum(1230123)).

int_pow_test() ->
    ?assertEqual(8, int_pow(2, 3)).

same_digits_test() ->
    ?assert(same_digits(2345, 2345)),
    ?assert(same_digits(2345, 3425)),
    ?assert(same_digits(234234, 234432)),
    ?assertNot(same_digits(123, 234)),
    ?assertNot(same_digits(123, 12)),
    ?assertNot(same_digits(123, 1234)),
    ?assertNot(same_digits(122, 121)).

all_permutations_test() ->
    ?assertEqual([123,132,213,231,312,321], lists:sort(number:all_permutations(123))),
    ?assertEqual([111], lists:sort(number:all_permutations(111))),
    ?assertEqual([133, 313, 331], lists:sort(number:all_permutations(133))).

nr_digits_test() ->
    ?assertEqual(3, nr_digits(554)),
    ?assertEqual(2, nr_digits(99)),
    ?assertEqual(1, nr_digits(7)),
    ?assertEqual(1, nr_digits(0)),
    ?assertEqual(1, nr_digits(-9)),
    ?assertEqual(2, nr_digits(-13)).

concat_test() ->
    ?assertEqual(1234, concat(12, 34)),
    ?assertEqual(123, concat(1, 23)),
    ?assertEqual(123, concat(12, 3)),
    ?assertEqual(99099, concat(990, 099)).
