-module(euler45).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

test(N) ->
  T = N*(N+1) div 2,
  case number:is_pentagonal(T) andalso number:is_hexagonal(T) of
    true  -> T;
    false -> test(N+1)
  end.

solve() ->
  test(286).
