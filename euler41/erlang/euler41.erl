-module(euler41).

-export([solve/0]).
 
-include_lib("eunit/include/eunit.hrl").

max_prime(Primes, Nr, Max) ->
  case primes:is_prime(Primes, Nr) of
    true  -> max(Nr, Max);
    false -> Max
  end.

max_prime_pandigital(Primes, NrDigits) ->
  pandigital:fold(fun(Pandigital, Max) -> max_prime(Primes, Pandigital, Max) end, 0, NrDigits).

% We know the pandigital number must have at least 4 digits (the example was 4 digits)
%
% We know that a number whose sum of digits is divisible by 3 is itself also divisible by 3, hence
%
% 1+2+3+4+5+6+7+8+9 = 45 is divisible by 3 => no pandigital is prime
% 1+2+3+4+5+6+7+8   = 36 is divisible by 3 => no pandigital is prime
% 1+2+3+4+5+6+7     = 28                   => pandigital could be prime
% 1+2+3+4+5+6       = 21 is divisible by 3 => no pandigital is prime
% 1+2+3+4           = 10                   => pandigital could be prime

solve() ->
  Primes = primes:compute_primes(9999999),
  max(max_prime_pandigital(Primes, 7), max_prime_pandigital(Primes, 4)).

  