%% Project Euler, problem 10
%% 
%% Find the sum of all the primes below two million.
%%
%% Approach e:
%% - Generate all primes below two million, using the sieve of Eratosthenes implemented using arrays.
%% - Only store odd numbers in the sieve array.
%% - Sum the generated primes. 

-module(euler10).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

init_sieve_array(N) ->
    Size = (N + 1) div 2,                                      % only store odd numbers (0->1, 1->3, 2->5, etc.)
    Array1 = array:new([{size, Size}, {default, is_prime}]),
    Array2 = array:set(0, is_not_prime, Array1),               % handle 1 as a special case
    Array2.

get_value_from_sieve_array(Nr, Array) ->
    1 = Nr rem 2,                                              % Nr must be odd
    Index = Nr div 2,
    array:get(Index, Array).

set_value_in_sieve_array(Nr, Value, Array) ->
    1 = Nr rem 2,                                              % Nr must be odd
    Index = Nr div 2,
    array:set(Index, Value, Array).

sieve(N) ->
    Array = init_sieve_array(N),
    MaxTry = trunc(math:sqrt(N)),
    Try = 3,                                                   % start with 3 as the first real prime
    sieve(N, Array, MaxTry, Try).

sieve(N, Array, MaxTry, Try) ->
    case Try > MaxTry of
        true ->
            Array;
        false ->
            case get_value_from_sieve_array(Try, Array) of
                is_not_prime ->
                    sieve(N, Array, MaxTry, Try + 2);
                is_prime ->
                    NewArray = remove_multiples_of_prime(N, Array, Try),
                    sieve(N, NewArray, MaxTry, Try + 2)
            end
    end.

remove_multiples_of_prime(N, Array, Prime) ->
    Multiple = 3 * Prime,
    remove_multiples_of_prime(N, Array, Prime, Multiple).

remove_multiples_of_prime(N, Array, Prime, Multiple) ->
    case Multiple > N of
        true -> 
            Array;
        false ->
            NewArray = set_value_in_sieve_array(Multiple, is_not_prime, Array),
            remove_multiples_of_prime(N, NewArray, Prime, Multiple + 2*Prime)
    end.
    
sum(Array) ->
    Sum = array:foldl(
        fun (Index, Value, Acc) -> 
            if 
                Value == is_prime ->
                    Acc + (2 * Index) + 1;
                true ->
                    Acc
            end
        end, 
        0, 
        Array
    ),
    Sum + 2.                 % 2 is the only even number and is not in the array

solve(N) -> sum(sieve(N)).

solve() -> solve(2000000).

solve_test() ->
    ?assertEqual(1060, solve(100)).
