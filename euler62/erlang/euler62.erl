-module(euler62).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

count_zeroes([]) -> 0;
count_zeroes([ 0 | Rest]) -> 1 + count_zeroes(Rest);
count_zeroes([ _ | Rest]) -> count_zeroes(Rest).

normalize(Nr) ->
  NrList = number:to_list(Nr),
  ZeroCount = count_zeroes(NrList),
  NormalizedNr = number:from_list(lists:sort(NrList)),
  {NormalizedNr, ZeroCount}.

try_n(N, Dict) ->
  Cube = N * N * N,
  NormalizedCube = normalize(Cube),
  case dict:find(NormalizedCube, Dict) of
    {ok, {LowestCube, Count}} ->
      case Count of 
        4 ->
          LowestCube;
        _ ->
          NewValue = {LowestCube, Count + 1},
          NewDict = dict:store(NormalizedCube, NewValue, Dict),
          try_n(N + 1, NewDict)
      end;
    error ->         
      NewValue = {Cube, 1},
      NewDict = dict:store(NormalizedCube, NewValue, Dict),
      try_n(N + 1, NewDict)
  end.

solve() ->
  Dict = dict:new(),
  try_n(1, Dict).

normalize_test() ->
  ?assertEqual(23348, normalize(34823)),
  ?assertEqual(2344, normalize(34024)).


