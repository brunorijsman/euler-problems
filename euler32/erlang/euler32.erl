%% Project Euler, problem 32
%%
%% We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for 
%% example, the 5-digit number, 15234, is 1 through 5 pandigital.
%%
%% The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing multiplicand, multiplier, and product 
%% is 1 through 9 pandigital.
%%
%% Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 
%% 9 pandigital.
%%
%% HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-module(euler32).

-export([solve/0, extract_nth/2, list_to_nr/1]).

extract_nth(N, List) ->
    Value = lists:nth(N, List),
    RemainingList = lists:sublist(List, N-1) ++ lists:nthtail(N, List),
    {Value, RemainingList}.

find_all_permutations_move_nth(PrefixList, RemainingList, N, PermutationsSoFar) ->
    {NewPrefixValue, NewRemainingList} = extract_nth(N, RemainingList),
    NewPrefixList = PrefixList ++ [NewPrefixValue],
    find_all_permutations(NewPrefixList, NewRemainingList, PermutationsSoFar).

find_all_permutations_move_up_to_nth(PrefixList, RemainingList, N, PermutationsSoFar) ->
    if
        N > length(RemainingList) ->
            PermutationsSoFar;
        true ->
            NewPermutations = find_all_permutations_move_nth(PrefixList, RemainingList, N, PermutationsSoFar),
            find_all_permutations_move_up_to_nth(PrefixList, RemainingList, N+1, NewPermutations)
    end.

find_all_permutations(PrefixList, _RemainingList = [], PermutationsSoFar) ->
    _NewPermutations = [PrefixList | PermutationsSoFar];

find_all_permutations(PrefixList, RemainingList, PermutationsSoFar) ->
    find_all_permutations_move_up_to_nth(PrefixList, RemainingList, 1, PermutationsSoFar).

find_all_permutations(List) ->
    find_all_permutations(_PrefixList = [], List, _PermutationsSoFar = []).

rev_list_to_nr([]) -> 
    0;

rev_list_to_nr([Digit | Rest]) ->
    Digit + 10 * rev_list_to_nr(Rest).
    
list_to_nr(List) ->
    rev_list_to_nr(lists:reverse(List)).

check_candidate_product_split(_LeftSide, Split, MaxSplit, _Product, ProductsSoFar) when Split > MaxSplit ->
    ProductsSoFar;

check_candidate_product_split(LeftSide, Split, MaxSplit, Product, ProductsSoFar) ->
    Factor1 = list_to_nr(lists:sublist(LeftSide, Split)),
    Factor2 = list_to_nr(lists:nthtail(Split, LeftSide)),
    NewProducts = case Factor1 * Factor2 == Product of
                      true -> 
                          io:format("~p * ~p = ~p~n", [Factor1, Factor2, Product]),
                          sets:add_element(Product, ProductsSoFar);
                      false -> 
                          ProductsSoFar
                  end,
    check_candidate_product_split(LeftSide, Split+1, MaxSplit, Product, NewProducts).

check_candidate_product(LeftSide, Product, ProductsSoFar) ->
    check_candidate_product_split(LeftSide, 1, length(LeftSide)-1, Product, ProductsSoFar).

find_all_products_for_permutation_split(_Permutation, Split, MaxSplit, ProductsSoFar) when Split > MaxSplit ->
    ProductsSoFar;

find_all_products_for_permutation_split(Permutation, Split, MaxSplit, ProductsSoFar) ->
    LeftSide = lists:sublist(Permutation, Split),
    Product = lists:nthtail(Split, Permutation),
    NewProducts = check_candidate_product(LeftSide, list_to_nr(Product), ProductsSoFar),
    find_all_products_for_permutation_split(Permutation, Split+1, MaxSplit, NewProducts).

find_all_products_for_permutation(Permutation, ProductsSoFar) ->
    find_all_products_for_permutation_split(Permutation, 1, length(Permutation)-1, ProductsSoFar).

find_all_products_for_permutation_list(_PermutationList = [], ProductsSoFar) ->
    ProductsSoFar;
    
find_all_products_for_permutation_list(_PermutationList = [Permutation | RestPermutationList], ProductsSoFar) ->
    NewProducts = find_all_products_for_permutation(Permutation, ProductsSoFar),
    find_all_products_for_permutation_list(RestPermutationList, NewProducts).
    
find_all_products_for_n(N, ProductsSoFar) ->
    PermutationList = find_all_permutations(lists:seq(1, N)),
    find_all_products_for_permutation_list(PermutationList, ProductsSoFar).

find_all_products_for_up_to_n(N, UpToN, ProductsSoFar) when N > UpToN ->
    ProductsSoFar;

find_all_products_for_up_to_n(N, UpToN, ProductsSoFar) ->
    NewProducts = find_all_products_for_n(N, ProductsSoFar),
    find_all_products_for_up_to_n(N+1, UpToN, NewProducts).

%% I originally misunderstood the problem statement and thought I was asked to consider
%% all pandigital number between length 1 and 9.  This is what I coded for. I kept the
%% general code, and simly changed the rage from 9 to 9 (i.e. only length 9).
%%
find_all_products() ->
    ProductsSet = find_all_products_for_up_to_n(9, 9, sets:new()),
    ProductsList = sets:to_list(ProductsSet),
    io:format("~nSet of unique solution products: ~p~n", [ProductsList]),
    Sum = lists:sum(ProductsList),
    io:format("Sum of products: ~p~n", [Sum]).

solve() ->
    find_all_products().
