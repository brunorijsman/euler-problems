%% Project Euler, problem 48
%% 
%% Find the last ten digits of 1^1 + 2^2 + ... + 1000^1000.

-module(euler48).

-export([solve/0]).

last_ten_digits(N) -> N rem 10000000000.

n_pow_n(N)      -> n_pow_n(N, N).
n_pow_n(N, 1)   -> N;
n_pow_n(N, Rem) -> N * n_pow_n(N, Rem-1).

sum_of_powers(Max) -> lists:sum([n_pow_n(N) || N <- lists:seq(1, Max)]).

solve() ->
    solve(1000).

solve(Max) ->
    last_ten_digits(sum_of_powers(Max)).
