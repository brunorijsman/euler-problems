-module(permutations).

-export([fold/3,
         all/1]).

-include_lib("eunit/include/eunit.hrl").

fold(Fun, Acc0, List) ->
  fold(Fun, Acc0, [], List).

fold(Fun, Acc0, Fixed, []) ->
  Fun(Fixed, Acc0);

fold(Fun, Acc0, Fixed, Remaining) ->
  fold(Fun, Acc0, Fixed, Remaining, length(Remaining)).

fold(_Fun, Acc0, _Fixed, _Remaining, 0) ->
  Acc0;

fold(Fun, Acc0, Fixed, Remaining, N) ->
  Move = lists:nth(N, Remaining),
  NewRemaining = lists:sublist(Remaining, N-1) ++ lists:nthtail(N, Remaining),
  NewFixed = Fixed ++ [Move],
  Acc1 = fold(Fun, Acc0, NewFixed, NewRemaining),
  fold(Fun, Acc1, Fixed, Remaining, N-1). 

enlist(Permutation, List) ->
  [Permutation | List].

all(List) ->
  fold(fun enlist/2, [], List).





