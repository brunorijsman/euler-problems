-module(primes).

-export([compute_primes/1,
         primes_to_list/1,
         is_prime/2, 
         foldl/3, 
         foldl/4, 
         is_prime/1, 
         are_all_primes/2,
         new_factorizer/1,
         factorize/2]).

-include_lib("eunit/include/eunit.hrl").

%% Compute all primes up to and including Max using the Sieve of Eratosthenes
%%
compute_primes(Max) ->
    Array = init_sieve_array(Max),
    MaxTry = trunc(math:sqrt(Max)),
    Try = 3,                                    % start with 3 as the first real prime
    sieve(Max, Array, MaxTry, Try).

%% Convert an opaque 'Primes' object to an ordered list of primes
%%
primes_to_list(Primes) ->
    lists:reverse(foldl(fun(P, L) -> [P | L] end, [], Primes)).

%% Check whether Nr is a prime, using the precomputed set of Primes
%%
is_prime(Primes, Nr) ->
    case Nr of
        2 -> true;
        _ -> case Nr rem 2 of
                 0 -> false;
                 1 -> array:get(Nr div 2, Primes)
             end
    end.

%% Apply an accumulator function to each prime in a set of pre-computed primes
%%
foldl(Fun, Acc0, Primes) ->
    foldl(Fun, Acc0, Primes, 2).

foldl(Fun, Acc0, Primes, StartPrime) ->
    case StartPrime > (2 * array:size(Primes) - 1) of
        false -> Acc = case is_prime(Primes, StartPrime) of
                           true  -> Fun(StartPrime, Acc0);
                           false -> Acc0
                       end,
                 foldl(Fun, Acc, Primes, StartPrime + 1);
        true  -> Acc0
    end.

%% Check whether Nr is prime, using the trial division method
%%
is_prime(Nr) ->
    if
        Nr == 1                       -> false;
        Nr /= 2 andalso Nr rem 2 == 0 -> false;
        Nr /= 3 andalso Nr rem 3 == 0 -> false;
        Nr /= 5 andalso Nr rem 5 == 0 -> false;
        Nr /= 7 andalso Nr rem 7 == 0 -> false;
        true                          -> is_prime_trial(Nr, 11, trunc(math:sqrt(Nr)))
    end.

is_prime_trial(Nr, Divisor, MaxDivisor) ->
    if
        Divisor > MaxDivisor -> true;
        Nr rem Divisor == 0  -> false;
        true                 -> is_prime_trial(Nr, Divisor + 2, MaxDivisor)
    end.

are_all_primes(_, []) ->
    true;

are_all_primes(Primes, [Nr | Rest]) ->
    case primes:is_prime(Primes, Nr) of
        true  -> are_all_primes(Primes, Rest);
        false -> false
    end.

init_sieve_array(Max) ->
    Size = (Max + 1) div 2,                                    % only store odd numbers (0->1, 1->3, 2->5, etc.)
    Array1 = array:new([{size, Size}, {default, true}]),
    Array2 = array:set(0, false, Array1),                      % handle 1 as a special case
    Array2.

get_value_from_sieve_array(Nr, Array) ->
    1 = Nr rem 2,                                              % Nr must be odd
    Index = Nr div 2,
    array:get(Index, Array).

set_value_in_sieve_array(Nr, Value, Array) ->
    1 = Nr rem 2,                                              % Nr must be odd
    Index = Nr div 2,
    array:set(Index, Value, Array).

sieve(Max, Array, MaxTry, Try) ->
    case Try > MaxTry of
        true ->
            Array;
        false ->
            case get_value_from_sieve_array(Try, Array) of
                false ->
                    sieve(Max, Array, MaxTry, Try + 2);
                true ->
                    NewArray = remove_multiples_of_prime(Max, Array, Try),
                    sieve(Max, NewArray, MaxTry, Try + 2)
            end
    end.

remove_multiples_of_prime(Max, Array, Prime) ->
    Multiple = 3 * Prime,
    remove_multiples_of_prime(Max, Array, Prime, Multiple).

remove_multiples_of_prime(Max, Array, Prime, Multiple) ->
    case Multiple > Max of
        true -> 
            Array;
        false ->
            NewArray = set_value_in_sieve_array(Multiple, false, Array),
            remove_multiples_of_prime(Max, NewArray, Prime, Multiple + 2*Prime)
    end.

-record(factorizer, {max, primes_list}).

%% Instantiate a new 'factorizer' which can factor numbers up to Max
%%
new_factorizer(Max) ->
    Primes = compute_primes(trunc(math:sqrt(Max)) + 1),
    #factorizer{max = Max, primes_list = primes_to_list(Primes)}.

%% Factor number 'Nr' into its prime factors using trial division by
%% precomputed primes.
%%
factorize(Nr, #factorizer{max = Max}) when Nr > Max ->
    erlang:error(nr_too_big_for_factorizer);

factorize(1, _) ->
    [1];

factorize(Nr, #factorizer{primes_list = Primes}) ->
    lists:reverse(factorize2(Nr, Primes, [])).

factorize2(1, _Primes, Factors) ->
    Factors;

factorize2(Nr, [], Factors) ->
    [Nr | Factors];

factorize2(Nr, AllPrimes = [Prime | MorePrimes], Factors) ->
    case Nr rem Prime of
        0 -> factorize2(Nr div Prime, AllPrimes, [Prime | Factors]);
        _ -> factorize2(Nr, MorePrimes, Factors)
    end.

primes_to_list_test() ->
    ?assertEqual([2, 3, 5, 7], primes_to_list(compute_primes(10))),
    ?assertEqual([2, 3, 5, 7, 11, 13, 17], primes_to_list(compute_primes(17))).

is_prime_2_test() ->
    Primes = compute_primes(20),
    ?assertNot(is_prime(Primes, 1)),
    ?assert(is_prime(Primes, 2)),
    ?assert(is_prime(Primes, 3)),
    ?assertNot(is_prime(Primes, 4)),
    ?assert(is_prime(Primes, 5)),
    ?assertNot(is_prime(Primes, 6)),
    ?assert(is_prime(Primes, 7)),
    ?assertNot(is_prime(Primes, 8)),
    ?assertNot(is_prime(Primes, 9)),
    ?assertNot(is_prime(Primes, 10)),
    ?assert(is_prime(Primes, 11)),
    ?assertNot(is_prime(Primes, 12)),
    ?assert(is_prime(Primes, 13)),
    ?assertNot(is_prime(Primes, 14)),
    ?assertNot(is_prime(Primes, 15)),
    ?assertNot(is_prime(Primes, 16)),
    ?assert(is_prime(Primes, 17)),
    ?assertNot(is_prime(Primes, 18)),
    ?assert(is_prime(Primes, 19)),
    ?assertNot(is_prime(Primes, 20)).

enlist(P, List) ->
    List ++ [P].

foldl_test() ->
    ?assertEqual([2, 3, 5, 7, 11, 13, 17, 19], primes:foldl(fun enlist/2, [], primes:compute_primes(19))).

is_prime_1_test() ->
    ?assertNot(is_prime(1)),
    ?assert(is_prime(2)),
    ?assert(is_prime(3)),
    ?assertNot(is_prime(4)),
    ?assert(is_prime(5)),
    ?assertNot(is_prime(6)),
    ?assert(is_prime(7)),
    ?assertNot(is_prime(8)),
    ?assertNot(is_prime(9)),
    ?assertNot(is_prime(10)),
    ?assert(is_prime(11)),
    ?assertNot(is_prime(12)),
    ?assert(is_prime(13)),
    ?assertNot(is_prime(14)),
    ?assertNot(is_prime(15)),
    ?assertNot(is_prime(16)),
    ?assert(is_prime(17)),
    ?assertNot(is_prime(18)),
    ?assert(is_prime(19)),
    ?assertNot(is_prime(20)),
    ?assert(is_prime(479001599)).

are_all_primes_test() ->
    Primes = primes:compute_primes(20),
    ?assert(are_all_primes(Primes, [3, 5, 7, 19])),
    ?assertNot(are_all_primes(Primes, [12, 3, 5, 7, 19])),
    ?assertNot(are_all_primes(Primes, [3, 5, 12, 7, 19])),
    ?assertNot(are_all_primes(Primes, [3, 5, 7, 19, 12])).

check_all_primes_precomputed(_Primes, 0) ->
    ok;

check_all_primes_precomputed(Primes, Max) ->
    _IsPrime = is_prime(Primes, Max),
    check_all_primes_precomputed(Primes, Max - 1).

check_all_primes_trial(0) ->
    ok;

check_all_primes_trial(Max) ->
    _IsPrime = is_prime(Max),
    check_all_primes_trial(Max - 1).

performance_test() ->
    Primes = ?debugTime("Compute 100,000 primes", compute_primes(100000)),
    ?debugTime("Check 100,000 primes (pre-computed)", check_all_primes_precomputed(Primes, 100000)),
    ?debugTime("Check 100,000 primes (by trial division)", check_all_primes_trial(100000)).

new_factorizer_test() ->
    _Factorizer = new_factorizer(100).

factorize_test() ->
    Factorizer100 = new_factorizer(100),
    ?assertError(nr_too_big_for_factorizer, factorize(101, Factorizer100)),
    ?assertEqual([1], factorize(1, Factorizer100)),
    ?assertEqual([2], factorize(2, Factorizer100)),
    ?assertEqual([3], factorize(3, Factorizer100)),
    ?assertEqual([17], factorize(17, Factorizer100)),
    ?assertEqual([2, 3], factorize(6, Factorizer100)),
    ?assertEqual([2, 2, 3], factorize(12, Factorizer100)),
    ?assertEqual([2, 3, 3], factorize(18, Factorizer100)),
    ?assertEqual([2, 2, 5, 5], factorize(100, Factorizer100)).
    

