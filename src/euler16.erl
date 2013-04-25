%% Project Euler, problem 16
%% 
%% What is the sum of the digits of the number 2^1000?

-module(euler16).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

power_of_2(N) ->
    1 bsl N.

digits_sum(N) ->
    if 
        N < 10 ->
            N;
        true ->
            (N rem 10) + digits_sum(N div 10)
    end.

solve() ->
    solve(1000).

solve(N) ->
    digits_sum(power_of_2(N)).

solve_test() ->
    ?assertEqual(1366, solve()).
