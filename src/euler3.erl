%% Project Euler, problem 3
%%
%% The prime factors of 13195 are 5, 7, 13 and 29.
%%
%% What is the largest prime factor of the number 600851475143 ?

-module(euler3).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
    solve(600851475143).

solve(N) ->
    solve(N, 2).

solve(N, Try) ->
    if
        Try > N div 2 ->
            N;
        true->
            if
                N rem Try == 0 ->
                    solve(N div Try, Try);
                true ->
                    solve(N, Try+1)
            end
    end. 

solve_test() ->
    ?assertEqual(6857, solve()).
