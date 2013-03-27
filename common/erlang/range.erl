-module(range).

-export([fold/4, sum/3, max/3]).

-include_lib("eunit/include/eunit.hrl").

fold(_Fun, Acc0, Start, End) when Start > End ->
  Acc0;

fold(Fun, Acc0, Start, End) ->
  Acc1 = Fun(Start, Acc0),
  fold(Fun, Acc1, Start + 1, End).

sum(Fun, Start, End) ->
  Sum = fun(Val, Acc) -> Acc + Fun(Val) end,
  fold(Sum, 0, Start, End).

max(Fun, Start, End) ->
  Max = fun(Val, Acc) -> erlang:max(Acc, Fun(Val)) end,
  fold(Max, 0, Start, End).

fold_test() ->
  Multiply = fun(Val, Product) -> Val * Product end,
  ?assertEqual(24, fold(Multiply, 1, 2, 4)).

sum_test() ->
  Double = fun(Val) -> 2 * Val end,
  ?assertEqual(18, sum(Double, 2, 4)).

max_test() ->
  LastDigit = fun(Nr) -> Nr rem 10 end,
  ?assertEqual(9, max(LastDigit, 18, 22)).
