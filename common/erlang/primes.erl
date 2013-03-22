-module(primes).

-export([compute_primes/1, is_prime/2, are_all_primes/2]).

-include_lib("eunit/include/eunit.hrl").

% Compute all primes up to and including Max using the Sieve of Eratosthenes
%
compute_primes(Max) ->
    Array = init_sieve_array(Max),
    MaxTry = trunc(math:sqrt(Max)),
    Try = 3,                                                   % start with 3 as the first real prime
    sieve(Max, Array, MaxTry, Try).

% Check whether Nr is a prime, using the precomputed set of Primes
%
is_prime(Primes, Nr) ->
  case Nr of
    2 -> true;
    _ -> case Nr rem 2 of
           0 -> false;
           1 -> array:get(Nr div 2, Primes)
         end
  end.

% Check whether Nr is prime, using the trial division method
%
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
  Primes = ?debugTime("Compute 1,000,000 primes", compute_primes(1000000)),
  ?debugTime("Check 1,000,000 primes (pre-computed)", check_all_primes_precomputed(Primes, 1000000)),
  ?debugTime("Check 500,000 primes (by trial division)", check_all_primes_trial(500000)).