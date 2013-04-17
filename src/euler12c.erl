%% Project Euler, problem 12
%% 
%% What is the value of the first triangle number to have over five hundred divisors?
%%
%% Implementation uses the Tau function.
%% See http://mathschallenge.net/index.php?section=faq&ref=number/number_of_divisors for details.

-module(euler12c).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

prime_factors(Number) ->
    prime_factors(Number, 2).

prime_factors(Number, Try) ->
    if
        Try > Number ->
            [];
        Number rem Try == 0 ->
            [Try | prime_factors(Number div Try, Try)];
        true ->
            prime_factors(Number, Try + 1)
    end.

make_count_list(List) ->
    make_count_list(List, []).

make_count_list([], Results) ->
    lists:reverse(Results);

make_count_list(List = [Head | _Tail], Results) ->
    {RemainingList, Count} = count_nr(List, Head),
    make_count_list(RemainingList, [Count | Results]).

count_nr(List = [Head | Tail], Nr) ->
    if 
        Head == Nr ->
            {RemainingList, Count} = count_nr(Tail, Nr),
            {RemainingList, Count + 1};
        true ->
            {List, 0}
    end;

count_nr([], _Nr) ->
    {[], 0}.

count_divisors(Number) ->
    CountList = make_count_list(prime_factors(Number)),
    lists:foldl(fun(Nr, Acc) -> Acc * (Nr + 1) end, 1, CountList).

solve() ->
    solve(500).

solve(WantedFactors) ->
    solve(1, 2, WantedFactors).

solve(Try, Step, WantedFactors) ->
    case count_divisors(Try) > WantedFactors of
        true ->
            Try;
        false ->
            solve(Try + Step, Step + 1, WantedFactors)
    end.

solve_test() ->
    ?assertEqual(120, solve(10)).
