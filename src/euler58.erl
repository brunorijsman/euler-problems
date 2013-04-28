-module(euler58).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

count_prime(N) ->
    case primes:is_prime(N) of
        true  -> 1;
        false -> 0
    end.

primes_in_spiral(Size) ->
    TopRight = Size*Size - 3*Size + 3,
    TopLeft = Size*Size - 2*Size + 2,
    BottomLeft = Size*Size - Size + 1,
    count_prime(TopRight) + count_prime(TopLeft) + count_prime(BottomLeft).

explore(Size, Primes, Diagonals, Fraction) ->
    NewPrimes = Primes + primes_in_spiral(Size),
    NewDiagonals = Diagonals + 4,
    case NewPrimes / NewDiagonals < Fraction of
        true  -> Size;
        false -> explore(Size + 2, NewPrimes, NewDiagonals, Fraction)
    end.    

solve(Fraction) ->
    explore(3, 0, 1, Fraction).

solve() ->
    solve(0.1).

solve_test() ->
    ?assertEqual(309, solve(0.2)).
