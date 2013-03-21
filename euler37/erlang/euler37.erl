-module(euler37).

-export([solve/0]).
 
-include_lib("eunit/include/eunit.hrl").

is_truncatable_prime(_Primes, Nr) when Nr < 10 ->
    false;

is_truncatable_prime(Primes, Nr) ->
    primes:are_all_primes(Primes, number:all_truncations_left(Nr)) andalso
    primes:are_all_primes(Primes, number:all_truncations_right(Nr)).    

sum_truncatable_primes(Max) ->
    Primes = primes:compute_primes(Max),
    sum_truncatable_primes(Primes, Max, 0).

sum_truncatable_primes(_Primes, Candidate, Sum) when Candidate < 10 ->
    Sum;

sum_truncatable_primes(Primes, Candidate, Sum) ->
    NewSum = case is_truncatable_prime(Primes, Candidate) of
                 true  -> io:format("~p is truncatable prime~n", [Candidate]),
                          Sum + Candidate;
                 false -> Sum
             end,
    sum_truncatable_primes(Primes, Candidate - 1, NewSum).  	     

solve(N) ->
    sum_truncatable_primes(N).

solve() ->
    {Time, Solution} = timer:tc(fun solve/1, [999999]),
    io:format("Solution = ~p~n", [Solution]),
    io:format("Run-time = ~p sec~n", [Time / 1000000.0]).

is_truncatable_prime_test() ->
    Primes = primes:compute_primes(4000),
    ?assert(is_truncatable_prime(Primes, 3797)),
    ?assertNot(is_truncatable_prime(Primes, 29)),
    ?assertNot(is_truncatable_prime(Primes, 7)).

solve_test() ->
    ?assertEqual(1986, solve(999)).
