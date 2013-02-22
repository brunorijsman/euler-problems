%% Project Euler, problem 9
%%
%% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which a^2 + b^2 = c^2
%%
%% For example 3^2 + 4^2 = 9 + 16 = 25 = 5^2
%%
%% There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.

-module(euler9).

-export([solve/0]).

is_solution(A,B,C) ->
    A*A + B*B == C*C andalso A + B + C == 1000.

solve(A,B,C) ->
   case is_solution(A,B,C) of
       true -> A * B * C;
       false ->
           if
               A+1 < B ->
                   solve(A+1, B, C);
               true ->
                   if
                       B+1 < C ->
                           solve (1, B+1, C);
                       true ->
                           solve (1, 2, C+1)
                   end
           end
   end.

solve() ->
   solve(1,2,3).
