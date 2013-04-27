%% Project Euler, problem 23
%% 
%% Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

-module(euler23).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

sum_of_divisors(N) -> 
    sum_of_divisors(N, 1, N div 2, 0).

sum_of_divisors(N, Try, MaxTry, Sum) ->
    if 
        Try > MaxTry ->
            Sum;
        N rem Try == 0 ->
            sum_of_divisors(N, Try + 1, MaxTry, Sum + Try);
        true ->
            sum_of_divisors(N, Try + 1, MaxTry, Sum)
    end.

is_abundant(N) ->
    sum_of_divisors(N) > N.

all_abundant_numbers_up_to(Max) ->
    [ N || N <- lists:seq(1, Max), is_abundant(N) ].

all_pairs_of_abundant_numbers_up_to(Max) ->
    AbundantNumbers = all_abundant_numbers_up_to(Max),
    [ N1 + N2 || N1 <- AbundantNumbers, N2 <- AbundantNumbers, N1 + N2 =< Max ].

all_non_pairs_up_to(Max) ->
    Pairs = all_pairs_of_abundant_numbers_up_to(Max),
    SortedPairs = lists:sort(Pairs),
    holes_in_list([ 0 | SortedPairs]).

holes_in_list([H1, H2 | Tail]) when H2 > H1 + 1 ->
    lists:seq(H1 + 1, H2 - 1) ++ holes_in_list([H2 | Tail]);
holes_in_list([_H1, H2 | Tail]) ->
    holes_in_list([H2 | Tail]);
holes_in_list(_List) ->
    [].

solve() -> 
    solve(28123).

solve(Max) ->
    lists:sum(all_non_pairs_up_to(Max)).

solve_test() ->
    ?assertEqual(240492, solve(1000)).
    
