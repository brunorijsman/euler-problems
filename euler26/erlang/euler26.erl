%% Project Euler, problem 26
%%
%% A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
%%
%% 1/2  =   0.5
%% 1/3  =   0.(3)
%% 1/4  =   0.25
%% 1/5  =   0.2
%% 1/6  =   0.1(6)
%% 1/7  =   0.(142857)
%% 1/8  =   0.125
%% 1/9	=   0.(1)
%% 1/10	=   0.1
%%
%% Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
%%
%% Find the value of d  1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

-module(euler26).

-export([solve/0, divide/2]).

recurrence(_Divident, []) ->
    [];

recurrence(Divident, Result) ->
    [{OldDivident, _} | OldResult] = Result,
    case OldDivident of
        Divident -> Result;
        _        -> recurrence(Divident, OldResult)
    end.

quotient({_Divident, Quotient}) -> $0 + Quotient.

quotients(Result) -> lists:map(fun quotient/1, Result).

print_result(Divident, Divisor, Result, Recurrence) ->
    FixedLength = length(Result) - length(Recurrence),
    Fixed = lists:sublist(Result, FixedLength),
    [ _ | Rest ] = Fixed,
    RestStr = quotients(Rest),
    RecurrenceStr = quotients(Recurrence),
    io:format("~p / ~p = 0.~s(~s)~n", [Divident, Divisor, RestStr, RecurrenceStr]).

divide(Divident, Divisor, Result) ->
    case recurrence(Divident, Result) of
        [] ->
            Quotient = Divident div Divisor,
            Remainder = Divident rem Divisor,
            NewResult = Result ++ [{Divident, Quotient}],
            divide(Remainder * 10, Divisor, NewResult);
        Recurrence ->
            {Result, Recurrence}
    end.

divide(Divident, Divisor) ->
    {Result, Recurrence} = divide(Divident, Divisor, []),
    print_result(Divident, Divisor, Result, Recurrence),
    length(Result).

reciprocal(Number) ->
    divide(1, Number).

solve(CurrentNumber, MaxNumber, BestNumber, _BestLength) when CurrentNumber > MaxNumber ->
    BestNumber;

solve(CurrentNumber, MaxNumber, BestNumber, BestLength) ->
    CurrentLength = reciprocal(CurrentNumber),
    case CurrentLength > BestLength of
        true  -> solve(CurrentNumber + 1, MaxNumber, CurrentNumber, CurrentLength);
        false -> solve(CurrentNumber + 1, MaxNumber, BestNumber, BestLength)
    end.

solve() ->
    solve(1, 1000, 0, 0).
