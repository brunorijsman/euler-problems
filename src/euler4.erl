%% Project Euler, problem 4
%%
%% A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers 
%% is 9009 = 91 × 99.
%% 
%% Find the largest palindrome made from the product of two 3-digit numbers.
%%
%% Use the dumb as a brick brute force approach: try all combinations of two 3-digit numbers, multiply them, check if 
%% the product is a palindrome, and remember the biggest one.

-module(euler4).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() -> solve(100, 100, 0, 999).

solve(N1, N2, BestSoFar, Max) ->
    Product = N1 * N2,
    NewBestSoFar = case (Product > BestSoFar) and number:is_palindrome(Product) of
        true  -> Product;
        false -> BestSoFar
    end,
    if
        N1 < Max -> solve(N1+1, N2, NewBestSoFar, Max);
        N1 == Max ->
            if
                N2 < Max -> solve(100, N2+1, NewBestSoFar, Max);
                N2 == Max -> NewBestSoFar 
            end
    end.

solve_test() ->
    ?assertEqual(36863, solve(100, 100, 0, 199)).
