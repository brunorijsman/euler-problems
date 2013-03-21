-module(euler39).

-export([solve/0]).

try_b(B, A, P, PSolutions) ->
  C = P - A - B,
  case (C >= B) andalso (A*A + B*B == C*C) of
    true ->
      PSolutions + 1;
    false ->
      PSolutions
  end.

try_a(A, P, PSolutions) ->
  range_fold(fun(B, Acc) -> try_b(B, A, P, Acc) end, PSolutions, A, P - A - 1).
  
try_p(P, {BestP, BestPSolutions}) ->
  PSolutions = range_fold(fun(A, Acc) -> try_a(A, P, Acc) end, 0, 1, P - 2),
  case PSolutions > BestPSolutions of
    true  -> {P, PSolutions};
    false -> {BestP, BestPSolutions}
  end.

range_fold(_Fun, Acc0, Start, End) when Start > End ->
  Acc0;

range_fold(Fun, Acc0, Start, End) ->
  range_fold(Fun, Fun(Start, Acc0), Start + 1, End).
    
solve() ->
  {BestP, _} = range_fold(fun try_p/2, {0, 0}, 1, 1000),
  BestP.