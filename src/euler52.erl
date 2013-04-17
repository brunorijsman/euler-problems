-module(euler52).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

is_solution(N) ->
  number:same_digits(N, 2*N) andalso
  number:same_digits(N, 3*N) andalso
  number:same_digits(N, 4*N) andalso
  number:same_digits(N, 5*N) andalso
  number:same_digits(N, 6*N).

try_n(N) ->
  case is_solution(N) of
    true  -> N;
    false -> try_n(N+1)
  end.

solve() ->
  try_n(1).

