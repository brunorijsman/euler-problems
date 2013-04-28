-module(euler55).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_ITERATIONS, 50).

solve() ->
    solve(9999).

solve(Max) ->
    count_lychrels(Max).

count_lychrels(Max) ->
    length([Nr || Nr <- lists:seq(1, Max), is_lychrel(Nr)]).

is_lychrel(Nr) ->
    is_lychrel(Nr, ?MAX_ITERATIONS).

is_lychrel(_Nr, 0) ->
    true;

is_lychrel(Nr, MoreIterations) ->
    NewNr = Nr + number:reverse(Nr),
    case number:is_palindrome(NewNr) of
        true  -> false;
        false -> is_lychrel(NewNr, MoreIterations - 1)
    end.

is_lychrel_test() ->
    ?assert(is_lychrel(196)),
    ?assertNot(is_lychrel(47)),
    ?assertNot(is_lychrel(349)),
    ?assert(is_lychrel(4994)),
    ?assert(is_lychrel(9999)).

solve_test() ->
    ?assertEqual(249, solve()).
    

