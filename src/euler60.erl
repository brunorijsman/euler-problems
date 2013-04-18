-module(euler60).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

compute_pair_table(MaxPrime) ->
    Primes = primes:compute_primes(MaxPrime),
    PairTable = dict:new(),
    compute_pair_table(Primes, 2, MaxPrime, PairTable).
    
compute_pair_table(Primes, _StartPrime, _MaxPrime, PairTable) ->
    {_, FinalPrimeTable} = primes:foldl(fun compute_pair_row/2, {Primes, PairTable}, Primes),
    FinalPrimeTable.
    
compute_pair_row(Prime, {Primes, PairTable}) ->
    {_, Pairs} = primes:foldl(fun consider_pair/2, {Prime, []}, Primes, Prime + 1),
    NewPairTable = case Pairs of
                       [] -> PairTable;
                       _  -> dict:append(Prime, Pairs, PairTable)
                   end,
    {Primes, NewPairTable}.

consider_pair(Prime2, {Prime1, Pairs}) ->
    GoodPair = primes:is_prime(number:concat(Prime1, Prime2)) andalso
        primes:is_prime(number:concat(Prime2, Prime1)),
    NewPairs = case GoodPair of
                   true  -> Pairs ++ [Prime2];
                   false -> Pairs
               end,
    {Prime1, NewPairs}.

find_set(_PairTable, SetSize, _MaxPrime, Set) when length(Set) == SetSize ->
    {ok, Set};

find_set(PairTable, SetSize, MaxPrime, _Set = []) ->
    find_set(PairTable, SetSize, MaxPrime, [], 2);

find_set(PairTable, SetSize, MaxPrime, Set) ->
    find_set(PairTable, SetSize, MaxPrime, Set, lists:last(Set) + 1).

find_set(_PairTable, _SetSize, MaxPrime, _Set, Addition) when Addition > MaxPrime ->
    no_solution;

find_set(PairTable, SetSize, MaxPrime, Set, Addition) ->
    Solution = case is_valid_addition(PairTable, Set, Addition) of
                   true  -> find_set(PairTable, SetSize, MaxPrime, Set ++ [Addition]);
                   false -> no_solution
               end,
    case Solution of
        {ok, _} ->
            Solution;
        no_solution ->
            find_set(PairTable, SetSize, MaxPrime, Set, Addition + 1)
    end.

is_valid_addition(_PairTable, _Set = [], _Addition) ->
    true;

is_valid_addition(PairTable, _Set = [First | Rest], Addition) ->
    case dict:find(First, PairTable) of
        {ok, [Pairs]} ->
            case lists:member(Addition, Pairs) of
                true ->
                    is_valid_addition(PairTable, Rest, Addition);
                false ->
                    false
            end;
        error ->
            false
    end.
  
sum(Value, Sum) ->
    Sum + Value.

solve(SetSize, MaxPrime) ->
    PairTable = compute_pair_table(MaxPrime),
    {ok, Set} = find_set(PairTable, SetSize, MaxPrime, []),
    Sum = lists:foldl(fun sum/2, 0, Set),
    {Set, Sum}.

solve() ->
    solve(5, 9999).

solve_test() ->
    ?assertEqual({[3, 7, 109, 673], 792}, solve(4, 999)).
