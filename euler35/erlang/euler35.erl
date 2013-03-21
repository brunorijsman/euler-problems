-module(euler35).

-export([solve/0]).
 
-include_lib("eunit/include/eunit.hrl").

nr_to_nines(Nr) ->
    NrList = number:to_list(Nr),    
    NinesList = lists:map(fun(_X) -> 9 end, NrList),
    number:from_list(NinesList).

is_circular_prime(Primes, Nr) ->
    primes:are_all_primes(Primes, number:all_rotations(Nr)).

count_circular_primes(Max) ->
    UpperBoundRotated = nr_to_nines(Max),
    Primes = primes:compute_primes(UpperBoundRotated),
    count_circular_primes(Primes, Max, 0).

count_circular_primes(_Primes, 1, Count) ->
    Count;

count_circular_primes(Primes, Candidate, Count) ->
    NewCount = case is_circular_prime(Primes, Candidate) of
                   true  -> Count + 1;
                   false -> Count
               end,
    count_circular_primes(Primes, Candidate - 1, NewCount).  	     

solve(N) ->
    count_circular_primes(N).

solve() ->
    {Time, Solution} = timer:tc(fun solve/1, [999999]),
    io:format("Solution = ~p~n", [Solution]),
    io:format("Run-time = ~p sec~n", [Time / 1000000.0]).

make_all_nines_test() ->
    ?assertEqual(9, nr_to_nines(1)).

is_circular_prime_test() ->
    Primes = primes:compute_primes(1000),
    ?assert(is_circular_prime(Primes, 2)),
    ?assert(is_circular_prime(Primes, 37)),
    ?assert(is_circular_prime(Primes, 197)).

count_circular_primes_test() ->
    ?assertEqual(13, count_circular_primes(99)).

solve_test() ->
    ?assertEqual(13, solve(99)).
