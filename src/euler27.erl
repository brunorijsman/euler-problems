-module(euler27).

-export([solve/0, is_prime/1]).

-include_lib("eunit/include/eunit.hrl").

is_prime(N) when N < 3 ->
    false;

is_prime(N) ->
    is_prime(N, 2, trunc(math:sqrt(N))).

is_prime(_N, Divisor, Bound) when Divisor > Bound ->
    true;

is_prime(N, Divisor, Bound) ->
    case N rem Divisor of
        0 -> false;
        _ -> is_prime(N, Divisor + 1, Bound)
    end.

range_fold(_Fun, Acc0, Start, End) when Start > End ->
    Acc0;

range_fold(Fun, Acc0, Start, End) ->
    range_fold(Fun, Fun(Start, Acc0), Start + 1, End).

max_n_prime_for_a_b(A, B) ->
    max_n_prime_for_a_b(A, B, 1).

max_n_prime_for_a_b(A, B, N) ->
    case is_prime(N * N + A * N + B) of
        true  -> max_n_prime_for_a_b(A, B, N + 1);
        false -> N - 1
    end.
            
a_b_candidate(A, B, Acc = {BestN, _BestAB}) ->
    N = max_n_prime_for_a_b(A, B),
    case N > BestN of 
        true  -> {N, A*B};
        false -> Acc
    end.

b_candidate(B, Acc0, Max) ->
    case is_prime(B) of
        true  -> range_fold(fun(A, Acc) -> a_b_candidate(A, B, Acc) end, Acc0, -B + 1, Max);
        false -> Acc0
    end.

solve(Max) ->
    {_, Product} = range_fold(fun(B, Acc) -> b_candidate(B, Acc, Max) end, {0, 0}, 2, Max),
    Product.

solve() ->
    solve(999).

solve_test() ->
    ?assertEqual(-1455, solve(99)).


        
