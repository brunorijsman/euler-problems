%% Project Euler, problem 7
%%
%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
%%
%% What is the 10001st prime number?

-module(euler7).

-export([solve/0]).

is_prime(N) ->
    MaxTry = trunc(math:sqrt(N)),
    is_prime(N, 2, MaxTry).

is_prime(_N, Try, MaxTry) when Try > MaxTry ->
    true;

is_prime(N, Try, MaxTry) ->
    if
        N rem Try == 0 ->
            false;
        true ->
            is_prime(N, Try+1, MaxTry)
    end.

find_nth_prime(PrimesNeeded) ->
    find_nth_prime(PrimesNeeded, 3, 1, 2).      %% Treat 2 as a special case: it is the only even prime.

find_nth_prime(PrimesNeeded, _Try, PrimesFound, LastPrimeFound) when PrimesFound == PrimesNeeded ->
    LastPrimeFound;

find_nth_prime(PrimesNeeded, Try, PrimesFound, LastPrimeFound) ->
    case is_prime(Try) of
        true -> find_nth_prime(PrimesNeeded, Try+2, PrimesFound+1, Try);
        false -> find_nth_prime(PrimesNeeded, Try+2, PrimesFound, LastPrimeFound)
    end.

solve() ->
    find_nth_prime(10001).
