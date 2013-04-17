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

-export([solve/0, reverse/1]).

reverse(N) -> reverse(N, 0).

reverse(0, Reversed) -> Reversed;
reverse(N, Reversed) -> reverse(N div 10, Reversed * 10 + N rem 10).

is_palindrome(N) -> N == reverse(N).

solve() -> solve(100, 100, 0).

solve(N1, N2, BestSoFar) ->
    Product = N1 * N2,
    case (Product > BestSoFar) and is_palindrome(Product) of
        true -> NewBestSoFar = Product;
        false -> NewBestSoFar = BestSoFar
    end,
    if
        N1 < 999 -> solve(N1+1, N2, NewBestSoFar);
        N1 == 999 ->
            if
                N2 < 999 -> solve(100, N2+1, NewBestSoFar);
                N2 == 999 -> NewBestSoFar 
            end
    end.
