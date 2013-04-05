-module(euler47).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

is_solution(Primes, N) ->
  is_composite(Primes, N) andalso is_odd(N) andalso not is_sum_of_prime_plus_square(Primes, N).

is_composite(Primes, N) ->
  not primes:is_prime(Primes, N).

is_odd(N) ->
  N rem 2 /= 0.

is_sum_of_prime_plus_square(Primes, N) ->
  try_sum(Primes, N, 2).

try_sum(_Primes, N, N) ->
  false;

try_sum(Primes, N, P) ->
  case primes:is_prime(Primes, P) andalso number:is_square((N-P) div 2) of
    true  -> true;
    false -> try_sum(Primes, N, P+1)
  end.

test(Primes, N) ->
  case is_solution(Primes, N) of
    true  -> N;
    false -> test(Primes, N+1)
  end.

solve() ->
  Primes = primes:compute_primes(1000000),
  test(Primes, 2).
