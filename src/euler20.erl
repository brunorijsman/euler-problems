%% Project Euler, problem 20
%% 
%% Find the sum of digits in 100!

-module(euler20).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

fact(0) -> 1;
fact(N) -> N * fact(N-1).

sum_of_digits(N) when N < 10 -> N;
sum_of_digits(N) -> (N rem 10) + sum_of_digits(N div 10).

solve() ->
    solve(100).

solve(N) ->
    sum_of_digits(fact(N)).

solve_test() ->
    ?assertEqual(648, solve()).
