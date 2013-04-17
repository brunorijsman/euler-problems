-module(euler1).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
   solve(999).

solve(N) when is_integer(N) ->
   if
       N == 0 ->
           0;
       N rem 3 == 0 ->
           N + solve(N-1);
       N rem 5 == 0 ->
           N + solve(N-1);
       true ->
           solve(N-1)
   end.

solve_test() ->
    ?assertEqual(233168, solve()).

