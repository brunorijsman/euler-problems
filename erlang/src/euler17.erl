%% Project Euler, problem 17
%% 
%% What is the sum of the digits of the number 2^1000?

-module(euler17).

-export([solve/0, solve/1, number_to_string/1, count_letters/1, count_letters_in_number/1]).

-define(SPACE, $\ ).

number_to_string(N) ->
    if
        N == 0 -> "zero";
        N == 1 -> "one";
        N == 2 -> "two";
        N == 3 -> "three";
        N == 4 -> "four";
        N == 5 -> "five";
        N == 6 -> "six";
        N == 7 -> "seven";
        N == 8 -> "eight";
        N == 9 -> "nine";
        N == 10 -> "ten";
        N == 11 -> "eleven";
        N == 12 -> "twelve";
        N == 13 -> "thirteen";
        N == 14 -> "fourteen";
        N == 15 -> "fifteen";
        N == 16 -> "sixteen";
        N == 17 -> "seventeen";
        N == 18 -> "eighteen";
        N == 19 -> "nineteen";
        N == 20 -> "twenty";
        N == 30 -> "thirty";
        N == 40 -> "forty";
        N == 50 -> "fifty";
        N == 60 -> "sixty";
        N == 70 -> "seventy";
        N == 80 -> "eighty";
        N == 90 -> "ninety";
        N < 100 -> number_to_string(N div 10 * 10) ++ "-" ++ number_to_string(N rem 10);
        N < 1000 -> 
            Hundreds = number_to_string(N div 100) ++ " hundred",
            if 
                (N rem 100) == 0 ->
                    Hundreds;
                true ->
                    Hundreds ++ " and " ++ number_to_string(N rem 100)
            end;
        N == 1000 -> "one thousand"
    end.

count_letters([]) ->
    0;
count_letters([H|T]) ->
    if
        (H == ?SPACE) or (H == $-) ->
            count_letters(T);
        true ->
            1 + count_letters(T)
    end.

count_letters_in_number(N) ->
    count_letters(number_to_string(N)).

solve() ->
    solve(1000).

solve(0) ->
    0;
solve(N) ->
    count_letters_in_number(N) + solve(N-1).
