-module(euler38).

-export([solve/0]).

list_to_nr(NrList) ->
    lists:foldl(fun(Digit, Acc) -> Digit + 10 * Acc end, 0, NrList).

nr_to_list(N) when N < 10 ->
    [N];

nr_to_list(N) ->
    nr_to_list(N div 10) ++ [N rem 10].

contains_digit(NrList, Digit, Contains) ->
    Contains andalso lists:any(fun(Element) -> Element == Digit end, NrList).

is_pandigital(NrList) ->
    if
        length(NrList) == 9 ->
            range_fold(fun(Digit, Contains) -> contains_digit(NrList, Digit, Contains) end, true, 1, 9);
        true ->
            false
    end.

consider_multiples(Base, Concatenation, NextMultiple, LargestPandigital) ->
    NewConcatenation = Concatenation ++ nr_to_list(Base * NextMultiple),
    if
        length(NewConcatenation) > 9 ->
            LargestPandigital;
        length(NewConcatenation) < 9 ->
            consider_multiples(Base, NewConcatenation, NextMultiple + 1, LargestPandigital);
        true ->
            case is_pandigital(NewConcatenation) of
                true ->
                    io:format("Base=~p NewConcatenation=~p NextMultiple=~p~n", [Base, NewConcatenation, NextMultiple]),
                    list_to_nr(NewConcatenation);
                false ->
                    LargestPandigital
            end
    end.

consider_base(Base, LargestPandigital) ->
    consider_multiples(Base, [], 1, LargestPandigital).

range_fold(_Fun, Acc0, Start, End) when Start > End ->
    Acc0;

range_fold(Fun, Acc0, Start, End) ->
    range_fold(Fun, Fun(Start, Acc0), Start + 1, End).
    
solve() ->
    range_fold(fun consider_base/2, 0, 1, 9999).
