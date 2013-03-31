-module(euler49).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

is_solution(Primes, N) ->
  N2 = N + 3330,
  N3 = N + 6660,
  primes:is_prime(Primes, N) andalso
  primes:is_prime(Primes, N2) andalso
  primes:is_prime(Primes, N3) andalso
  number:same_digits(N, N2) andalso
  number:same_digits(N, N3) andalso
  N /= 1487.

try_n(_Primes, N) when N > 9999 ->
  undefined;

try_n(Primes, N) ->
  case is_solution(Primes, N) of
    true  -> N;
    false -> try_n(Primes, N + 1)
  end.

solve() ->
  Primes = primes:compute_primes(9999 + 6660),
  N = try_n(Primes, 1000),
  io:format("~p~p~p~n", [N, N+3330, N+6660]),
  ok.

