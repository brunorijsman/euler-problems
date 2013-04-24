-module(euler82).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
    Square = euler81common:read_square("../input/euler81.txt", 80),
    solve(Square, 80).

solve(Square, Size) ->
    Costs = dict:new(),
    NewCosts = lists:foldl(fun(X, C) -> solve_col(Square, C, Size, X) end, Costs, lists:seq(1, Size)),
    lists:min([dict:fetch({Size, Y}, NewCosts) || Y <- lists:seq(1, Size)]).
    
solve_col(Square, Costs, Size, X) ->
    Costs1 = lists:foldl(fun(Y, C) -> go_right(Square, Size, C, X, Y) end, Costs, lists:seq(1, Size)),
    Costs2 = lists:foldl(fun(Y, C) -> try_down(Square, Size, C, X, Y) end, Costs1, lists:seq(2, Size)),
    lists:foldr(fun(Y, C) -> try_up(Square, Size, C, X, Y) end, Costs2, lists:seq(1, Size-1)).

cget(Costs, Size, X, Y) when (X >= 1) andalso (X =< Size) andalso (Y >= 1) andalso (Y =< Size) ->
    dict:fetch({X, Y}, Costs);

cget(_Costs, _Size, _X, _Y) ->
    0.

sget(Square, X, Y) ->
    euler81common:get(Square, X, Y).

go_right(Square, Size, Costs, X, Y) ->
    Cost = cget(Costs, Size, X-1, Y) + sget(Square, X, Y),
    dict:store({X, Y}, Cost, Costs).

try_down(Square, Size, Costs, X, Y) ->
    OldCost = cget(Costs, Size, X, Y),
    TryCost = cget(Costs, Size, X, Y-1) + sget(Square, X, Y),
    Cost = min(OldCost, TryCost),
    dict:store({X, Y}, Cost, Costs).
    
try_up(Square, Size, Costs, X, Y) ->
    OldCost = cget(Costs, Size, X, Y),
    TryCost = cget(Costs, Size, X, Y+1) + sget(Square, X, Y),
    Cost = min(OldCost, TryCost),
    dict:store({X, Y}, Cost, Costs).
    
solve_test() ->
    {Square, Size} = euler81common:get_test_square(),
    ?assertEqual(994, solve(Square, Size)).
