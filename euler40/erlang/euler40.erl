-module(euler40).

-export([solve/0, nr_to_rev_list/1]).

nr_to_rev_list(Nr) when Nr == 0 ->
  [];

nr_to_rev_list(Nr) ->
  [ Nr rem 10 | nr_to_rev_list(Nr div 10) ].  

make_fraction(ReverseFraction, Nr, MoreDigitsNeeded) when MoreDigitsNeeded > 0 ->
  NrRevList = nr_to_rev_list(Nr),
  make_fraction(NrRevList ++ ReverseFraction, Nr + 1, MoreDigitsNeeded - length(NrRevList));

make_fraction(ReverseFraction, _Nr, _MoreDigitsNeeded) ->
  lists:reverse(ReverseFraction).
  
make_fraction(DigitsNeeded) ->
  make_fraction([], 1, DigitsNeeded).

solve() ->
  Fraction = make_fraction(1000000),
  D1 = lists:nth(1, Fraction),
  D10 = lists:nth(10, Fraction),
  D100 = lists:nth(100, Fraction),
  D1000 = lists:nth(1000, Fraction),
  D10000 = lists:nth(10000, Fraction),
  D100000 = lists:nth(100000, Fraction),
  D1000000 = lists:nth(1000000, Fraction),
  D1 * D10 * D100 * D1000 * D10000 * D100000 * D1000000.
