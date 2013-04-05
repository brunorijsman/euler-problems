-module(euler53).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

count_n_r(N, R) ->
  case combinations:count(N, R) > 1000000 of
    true  -> 1;
    false -> 0
  end.

count_n(N) ->
  range:sum(fun(R) -> count_n_r(N, R) end, 1, N).  

solve() ->
  range:sum(fun count_n/1, 1, 100).

