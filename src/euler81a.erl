-module(euler81a).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
    Square = euler81common:read_square("../input/euler81.txt", 80),
    solve(Square, 80).

solve(Square, Size) ->
    Costs = dict:new(),
    NewCosts = lists:foldr(fun(Y, C) -> solve_row(Square, C, Size, Y) end, Costs, lists:seq(1, Size)),
    dict:fetch({1, 1}, NewCosts).
    
solve_row(Square, Costs, Size, Y) ->
    lists:foldr(fun(X, C) -> solve_pos(Square, C, Size, X, Y) end, Costs, lists:seq(1, Size)).

solve_pos(Square, Costs, Size, X, Y) ->
    PosCost = euler81common:get(Square, X, Y),
    dict:store({X, Y}, PosCost + pos_add(Costs, Size, X, Y), Costs).

pos_add(_Costs, Size, X, Y) when (X == Size) andalso (Y == Size) ->
    0;

pos_add(Costs, Size, X, Y) when (X == Size) ->
    dict:fetch({X, Y + 1}, Costs);

pos_add(Costs, Size, X, Y) when (Y == Size) ->
    dict:fetch({X + 1, Y}, Costs);

pos_add(Costs, _Size, X, Y) ->
    YAdd = dict:fetch({X, Y + 1}, Costs),
    XAdd = dict:fetch({X + 1, Y}, Costs),
    min(XAdd, YAdd).

solve_test() ->
    {Square, Size} = euler81common:get_test_square(),
    ?assertEqual(2427, solve(Square, Size)).
