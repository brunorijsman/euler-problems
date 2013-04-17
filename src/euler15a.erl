%% Project Euler, problem 15
%%
%% Find number of corner-to-corner routes (without backtracking) in a 20x20 grid

-module(euler15a).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

routes_in_grid(X, Y) ->
    if
        (X == 0) or (Y == 0) ->
            1;
        true ->
            routes_in_grid(X - 1, Y) + routes_in_grid(X, Y - 1)
    end.

solve() ->
    solve(20).

solve(N) ->
    routes_in_grid(N, N).
