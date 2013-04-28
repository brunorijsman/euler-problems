%% Project Euler, problem 6
%%
%% The sum of the squares of the first ten natural numbers is,
%% 1^2 + 2^2 + ... + 10^2 = 385
%% 
%% The square of the sum of the first ten natural numbers is,
%% (1 + 2 + ....+ 10)^2 = 55^2 =3025
%% 
%% Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum 
%% is 3025 - 385 = 2640.
%% 
%% Find the difference between the sum of the squares of the first one hundred natural numbers and the square of 
%% the sum.

-module(euler6).

-include_lib("eunit/include/eunit.hrl").

-export([solve/0]).

sum_of_squares(0) -> 0;
sum_of_squares(N) -> N * N + sum_of_squares(N-1).

square_of_sum(N) ->
    Sum = N * (N+1) div 2,
    Sum * Sum.

solve() -> solve(100).

solve(N) -> square_of_sum(N) - sum_of_squares(N).

solve_test() ->
    ?assertEqual(25164150, solve()).
