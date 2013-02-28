-module(euler29).

-export([solve/0, pow/2]).

pow(_A, 0) -> 1;
pow(A, B)  -> A * pow(A, B-1). 

a_b_candidate(A, B, Terms) ->
    sets:add_element(pow(A, B), Terms).

a_candidate(A, Terms0) ->
    range_fold(fun(B, Terms) -> a_b_candidate(A, B, Terms) end, Terms0, 2, 100).

range_fold(_Fun, Acc0, Start, End) when Start > End ->
    Acc0;

range_fold(Fun, Acc0, Start, End) ->
    range_fold(Fun, Fun(Start, Acc0), Start + 1, End).

solve() ->
    Terms = range_fold(fun a_candidate/2, sets:new(), 2, 100),
    sets:size(Terms).



             

