-module(euler56).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

max_a_b(A, B) ->
  number:digit_sum(number:int_pow(A, B)).

max_a(A) ->
  range:max(fun(B) -> max_a_b(A, B) end, 1, 100).

solve() ->
  range:max(fun max_a/1, 1, 100).

