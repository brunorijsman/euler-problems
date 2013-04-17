
-module(euler43).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

is_substring_divisible(NrList) ->
  [_D1, D2, D3, D4, D5, D6, D7, D8, D9, D10] = NrList,
  (D2*100 + D3*10 + D4) rem 2 == 0 andalso
  (D3*100 + D4*10 + D5) rem 3 == 0 andalso
  (D4*100 + D5*10 + D6) rem 5 == 0 andalso
  (D5*100 + D6*10 + D7) rem 7 == 0 andalso
  (D6*100 + D7*10 + D8) rem 11 == 0 andalso
  (D7*100 + D8*10 + D9) rem 13 == 0 andalso
  (D8*100 + D9*10 + D10) rem 17 == 0.

sum(NrList, Acc) ->
  case is_substring_divisible(NrList) of
    true  -> Acc + number:from_list(NrList);
    false -> Acc
  end.

solve() ->
  permutations:fold(fun sum/2, 0, lists:seq(0, 9)).

is_substring_divisible_test() ->
  ?assert(is_substring_divisible([1, 4, 0, 6, 3, 5, 7, 2, 8, 9])),
  ?assertNot(is_substring_divisible([1, 2, 3, 4, 5, 6, 7, 8, 9, 0])).