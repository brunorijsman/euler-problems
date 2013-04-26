%% Project Euler, problem 21
%% 
%% Evaluate the sum of all amicable pairs under 10000.

-module(euler21).

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

is_amicable(N) ->
    CandidateOther = sum_of_divisors(N),
    ShouldBeN = sum_of_divisors(CandidateOther),
    (N == ShouldBeN) and (N /= CandidateOther).

sum_of_amicable_numbers(N) ->
    sum_of_amicable_numbers(1, N, 0).

sum_of_amicable_numbers(Try, MaxTry, Sum) ->
    IsAmicable = is_amicable(Try), 
    if 
        Try >= MaxTry ->
            Sum;
        IsAmicable ->
            sum_of_amicable_numbers(Try + 1, MaxTry, Sum + Try);
        true ->
            sum_of_amicable_numbers(Try + 1, MaxTry, Sum)
    end.

solve() -> 
    solve(10000).

solve(N) -> 
    sum_of_amicable_numbers(N).

solve_test() ->
    ?assertEqual(504, solve(1000)).
