%% Project Euler, problem 25
%% 
%% What is the first term in the Fibonacci sequence to contain 1000 digits?

-module(euler25).

-export([solve/0]).

nr_digits(N) ->
    if 
        N < 10 ->
            1;
        true ->
            1 + nr_digits(N div 10)
    end.

fibonacci(WantedDigits) ->
    fibonacci(WantedDigits, 1, 1, 2).

fibonacci(WantedDigits, Prev, Current, Term) ->
    NrDigits = nr_digits(Current),
    if
        NrDigits >= WantedDigits ->
            Term;
        true ->
            fibonacci(WantedDigits, Current, Prev + Current, Term + 1)
    end.

solve() ->
    solve(1000).

solve(WantedDigits) ->
    fibonacci(WantedDigits).
