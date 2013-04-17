-module(euler104).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

nr_to_list_rev(N) when N < 10 ->
    [N];

nr_to_list_rev(N) ->
    [N rem 10 | nr_to_list_rev(N div 10)].

nr_to_list(N) ->
    lists:reverse(nr_to_list_rev(N)).

is_pandigital(N) ->
    NList = nr_to_list(N),
    lists:sort(NList) == [1, 2, 3, 4, 5, 6, 7, 8, 9].

last_9_digits(N) ->
    N rem 1000000000.

first_9_digits(N) when N >= 100000000000000000 ->
    first_9_digits(N div 1000000000);

first_9_digits(N) when N >= 1000000000 ->
    first_9_digits(N div 10);

first_9_digits(N) -> 
    N.

is_solution(N) when N < 100000000 ->
    false;

is_solution(N) ->
    is_pandigital(last_9_digits(N)) andalso is_pandigital(first_9_digits(N)).

consider(K, Fib, PrevFib) ->
    case is_solution(Fib) of
        true  -> K;
        false -> consider(K+1, Fib + PrevFib, Fib)
    end.    

solve() ->
    consider(2, 1, 1).

is_solution_test() ->
    ?assert(is_solution(1234657891111987654231)),
    ?assertNot(is_solution(1234567881111987654321)),
    ?assertNot(is_solution(1234567891111987654322)),
    ?assertNot(is_solution(12345678)).
    

