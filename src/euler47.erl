-module(euler47).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(MAX, 1000000).   %% Consider numbers up to MAX

solve() ->
    solve(4).

solve(UniqueFactors) ->
    Factorizer = primes:new_factorizer(?MAX),
    consider(Factorizer, UniqueFactors, [{1, UniqueFactors}, {2, 1}, {3, 1}, {4, 2}]).

consider(_Factorizer, UniqueFactors, [{_N4, UniqueFactors}, {_N3, UniqueFactors}, {_N2, UniqueFactors}, {N1, UniqueFactors}]) ->
        N1;
      
consider(Factorizer, UniqueFactors, [P4 = {N4, _}, P3, P2, _P1]) ->
        N5 = N4 + 1,
        F5 = length(lists2:uniq(primes:factorize(N5, Factorizer))),
        consider(Factorizer, UniqueFactors, [{N5, F5}, P4, P3, P2]).

solve_test() ->
    ?assertEqual(1308, solve(3)).
    



