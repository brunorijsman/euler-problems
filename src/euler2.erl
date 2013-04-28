-module(euler2).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
    solve(4000000).

solve(Max) ->
    solve(Max, 1, 1, 0).

solve(Max, Current, Prev, Total) ->
    if
        Current > Max ->
            Total;
        Current rem 2 == 0 ->
            solve(Max, Current+Prev, Current, Total+Current);
        true ->
            solve(Max, Current+Prev, Current, Total)
    end.

solve_test() ->
    ?assertEqual(4613732, solve()).
