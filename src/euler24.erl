%% Project Euler, problem 24
%% 
%% What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
%%
%% Note that we compute ALL permutations of the 10 digits even though we only need the first million. I can't figure
%% out an easy way to "break off the comprehension early" and I thought the code using comprehension was too elegant
%% to pass up. This algorithm still finishes well within the allowed 1 minute.

-module(euler24).

-export([solve/0]).

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])]. 

solve() -> lists:nth(1000000, permutations("0123456789")).
