%% Project Euler, problem 15
%%
%% Find number of corner-to-corner routes (without backtracking) in a 20x20 grid

-module(euler15b).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

routes_in_grid(X, Y) ->
    Solutions = dict:new(),
    {Routes, _} = routes_in_grid(X, Y, Solutions),
    Routes.

routes_in_grid(0, _Y, Solutions) ->
    {1, Solutions};

routes_in_grid(_X, 0, Solutions) ->
    {1, Solutions};

routes_in_grid(X, Y, Solutions) ->
    case dict:find({X, Y}, Solutions) of
        {ok, Routes} ->
            {Routes, Solutions};
        error ->
            {Routes1, Solutions1} = routes_in_grid(X - 1, Y, Solutions),
            {Routes2, Solutions2} = routes_in_grid(X, Y - 1, Solutions1),
            NewRoutes = Routes1 + Routes2,
            NewSolutions = dict:store({X, Y}, NewRoutes, Solutions2),
            {NewRoutes, NewSolutions}
    end.

solve() ->
    solve(20).

solve(N) ->
    routes_in_grid(N, N).

solve_test() ->
    ?assertEqual(20, solve(3)).
