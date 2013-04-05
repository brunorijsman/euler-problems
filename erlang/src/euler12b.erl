%% Project Euler, problem 12
%% 
%% What is the value of the first triangle number to have over five hundred divisors?

-module(euler12b).

-export([solve/0, solve/1]).

%% Try only the divisors between 1 and sqrt(Number).
%%
count_divisors(Number) ->
    Try = 1,
    MaxTry = trunc(math:sqrt(Number)),
    Count = 0,
    count_divisors(Number, Try, MaxTry, Count).

count_divisors(Number, Try, MaxTry, Count) ->
    if 
        Try > MaxTry ->
            Count;
        Number rem Try == 0 ->
            if 
                Try == MaxTry ->
                    Found = 1;
                true ->
                    Found = 2      % every divisor below sqrt(Number) has a counter-part above it
            end,
            count_divisors(Number, Try + 1, MaxTry, Count + Found);
        true ->
            count_divisors(Number, Try + 1, MaxTry, Count)
    end.

solve() ->
    solve(500).

solve(WantedFactors) ->
    solve(3, 3, 0, WantedFactors).

solve(Try, Step, BestSoFar, WantedFactors) ->
    Count = count_divisors(Try),
    if
        Count > BestSoFar ->
            NewBestSoFar = Count;
        true ->
            NewBestSoFar = BestSoFar
    end,
    case Count > WantedFactors of
        true ->
            Try;
        false ->
            solve(Try + Step, Step + 1, NewBestSoFar, WantedFactors)
    end.
