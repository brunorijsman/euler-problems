%% Project Euler, problem 36
%%
%% The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
%%
%% Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
%%
%% (Please note that the palindromic number, in either base, may not include leading zeros.)

-module(euler36).

-export([solve/0, to_binary/1, to_number/1, is_palindrome/1]).

is_palindrome([]) -> true;

is_palindrome([_]) -> true;

is_palindrome([First | Rest]) ->
    {Middle, [Last]} = lists:split(length(Rest) - 1, Rest),
    case First of
        Last -> is_palindrome(Middle);
        _    -> false
    end.

to_binary(Number) -> 
    to_binary(Number, []).

to_binary(0, Result) -> 
    Result;

to_binary(N, Result) -> 
    to_binary(N div 2, [N rem 2 | Result]).

to_number(DigitList) ->
    lists:foldl(fun(Digit, Acc) -> Digit + 10 * Acc end, 0, DigitList).

to_string(DigitList) ->
    lists:map(fun(Digit) -> Digit + $0 end, DigitList).

combinations_fold(Fun, Acc0, Alphabet, Length) -> 
    combinations_fold(Fun, Acc0, Alphabet, Length, []).

combinations_fold(Fun, Acc0, _Alphabet, 0, Base) ->
    Fun(Base, Acc0);

combinations_fold(Fun, Acc0, Alphabet, MoreLength, Base) ->
    ListFun = fun(Letter, Acc) -> combinations_fold(Fun, Acc, Alphabet, MoreLength - 1, Base ++ [Letter]) end,
    lists:foldl(ListFun, Acc0, Alphabet).

make_palindrome(Half, false) ->
    Half ++ lists:reverse(Half);

make_palindrome(Half, true) ->
    Half ++ lists:reverse(lists:sublist(Half, length(Half) - 1)).

palindromes_fold(Fun, Acc0, Alphabet, Length) ->
    case Length rem 2 of
        0 -> palindromes_fold(Fun, Acc0, Alphabet, Length div 2, false);
        1 -> palindromes_fold(Fun, Acc0, Alphabet, Length div 2 + 1, true)
    end.

palindromes_fold(Fun, Acc0, Alphabet, HalfLength, Odd) ->
    combinations_fold(fun(C, Acc) -> Fun(make_palindrome(C, Odd), Acc) end, Acc0, Alphabet, HalfLength).

has_leading_zero([])      -> true;
has_leading_zero([0 | _]) -> true;
has_leading_zero(_)       -> false.

palindrome_candidate(NumberList, Sum) ->
    Number = to_number(NumberList),
    BinaryList = to_binary(Number),
    case not has_leading_zero(NumberList) andalso is_palindrome(BinaryList) of
        true ->
            io:format("Number=~p Binary=~p~n", [to_string(NumberList), to_string(BinaryList)]),
            Sum + Number;
        false ->
            Sum
    end.

sum_for_length(Length) ->
    Digits = lists:seq(0,9),
    palindromes_fold(fun palindrome_candidate/2, 0, Digits, Length).

solve() ->
    io:format("Sum is ~p~n", [lists:foldl(fun(L, A) -> A + sum_for_length(L) end, 0, lists:seq(1,6))]).
