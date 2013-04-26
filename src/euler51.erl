-module(euler51).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

-define(BOUND, 999).

solve() ->
  solve(8, ?BOUND).

solve(FamilySize, Bound) ->
    Primes = primes:compute_primes(Bound),
    PrimeList = primes:primes_to_list(Primes),
    Nrs = lists:seq(1, Bound),
    {_, Solution} = lists2:find_solution(fun(Prime) -> try_nr(Primes, FamilySize, Prime) end, PrimeList),
    Solution.

try_nr(Primes, FamilySize, Nr) ->
    Digits = number:nr_digits(Nr),
    lists2:find_solution(fun(D) -> try_nr_digits(Primes, FamilySize, Nr, D) end, lists:seq(1, Digits-1)).

try_nr_digits(Primes, FamilySize, Nr, Digits) ->
    CandidatePositions = combinations:all(lists:seq(1, number:nr_digits(Nr)), Digits),
    lists2:find_solution(fun(P) -> try_nr_positions(Primes, FamilySize, Nr, P) end, CandidatePositions).

try_nr_positions(Primes, FamilySize, Nr, Positions) ->
    PrimeFamily = prime_family(Primes, Nr, Positions),
    case length(PrimeFamily) of
        FamilySize -> 
            Prime = lists:min(PrimeFamily),
            {true, Prime};
        _ -> 
            false
    end.

prime_family(Primes, Nr, Positions) ->
    Family = family(Nr, Positions),
    lists:filter(fun(M) -> primes:is_prime(Primes, M) end, Family).

family(Prime, Positions) ->
    lists:foldl(fun(D, F) -> collect_family(Prime, Positions, D, F) end, [], lists:seq(0, 9)).

contains_1(List) ->
    lists:any(fun(E) -> E == 1 end, List).
                      
collect_family(Prime, Positions, Digit, Family) ->
    %% Don't change the first digit to 0 as it changes the number of digits in the number
    case (Digit == 0) andalso (contains_1(Positions)) of
        true ->
            Family;
        false ->
            FamilyMember = change_digits(Prime, Positions, Digit),
            [FamilyMember | Family]
    end.

change_digits(Nr, Positions, Digit) ->
    lists:foldl(fun(Elem, Acc) -> change_digit(Acc, Elem, Digit) end, Nr, Positions).

change_digit(Nr, Pos, Digit) ->
    NrList = number:to_list(Nr),
    NewNrList = lists:sublist(NrList, Pos-1) ++ 
        [Digit] ++ 
        lists:sublist(NrList, Pos+1, length(NrList) - Pos),
    number:from_list(NewNrList).

solve_test() ->
    ?assertEqual(13, solve(6, 99)),
    ?assertEqual(56003, solve(7, 99999)).

