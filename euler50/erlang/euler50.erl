-module(euler50).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

sum_of_primes(Primes, Start, MaxSum) ->
  sum_of_primes(Primes, Start, Start, MaxSum, 0, 0, 0, 0).

sum_of_primes(_Primes, _Start, Next, MaxSum, _Sum, _Length, BestSum, BestLength) when Next > MaxSum ->
  {BestLength, BestSum};

sum_of_primes(Primes, Start, Next, MaxSum, Sum, Length, BestSum, BestLength) ->
  case primes:is_prime(Primes, Next) of
    false ->
      sum_of_primes(Primes, Start, Next + 1, MaxSum, Sum, Length, BestSum, BestLength);
    true ->
      NewSum = Sum + Next,
      NewLength = Length + 1,
      case NewSum > MaxSum of
        true ->
          {BestLength, BestSum};
        false ->
          case primes:is_prime(Primes, NewSum) of
            false ->
              sum_of_primes(Primes, Start, Next + 1, MaxSum, NewSum, NewLength, BestSum, BestLength);
            true ->
              case NewLength > BestLength of
                false ->
                  sum_of_primes(Primes, Start, Next + 1, MaxSum, NewSum, NewLength, BestSum, BestLength);
                true ->
                  sum_of_primes(Primes, Start, Next + 1, MaxSum, NewSum, NewLength, NewSum, NewLength)
              end
          end
      end
  end.

try_start(_Primes, Start, Max, BestLength, BestSum) when Start > Max ->
  {BestLength, BestSum};

try_start(Primes, Start, Max, BestLength, BestSum) ->
  {BestLengthForStart, BestSumForStart} = case primes:is_prime(Primes, Start) of
    true  -> sum_of_primes(Primes, Start, Max);
    false -> {0, 0}
  end,
  {NewBestLength, NewBestSum} = case BestLengthForStart > BestLength of
    true  -> {BestLengthForStart, BestSumForStart};
    false -> {BestLength, BestSum}
  end,
  try_start(Primes, Start + 1, Max, NewBestLength, NewBestSum).

solve(Max) ->
  Primes = primes:compute_primes(Max),
  try_start(Primes, 1, Max, 0, 0).

solve() ->
  solve(1000000).

solve_test() ->
  ?assertEqual({6, 41}, solve(100)),
  ?assertEqual({21, 953}, solve(1000)).