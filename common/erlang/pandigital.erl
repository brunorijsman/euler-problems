-module(pandigital).

-export([is/1, all/1]).

-include_lib("eunit/include/eunit.hrl").

is(Nr) ->
  NrList = number:to_list(Nr),
  NrList == lists:reverse(NrList).

all(Nr) ->
  lists:map(fun number:from_list/1, permutations:all(lists:seq(1, Nr))).

is_test() ->
  ?assert(is(0)),
  ?assert(is(1)),
  ?assert(is(9)),
  ?assert(is(11)),
  ?assert(is(22)),
  ?assert(is(111)),
  ?assert(is(121)),
  ?assert(is(123321)),
  ?assert(is(1239321)),
  ?assertNot(is(12)),
  ?assertNot(is(12320)),
  ?assertNot(is(12329)),
  ?assertNot(is(1231)),
  ?assertNot(is(1291)).

all_test() ->
  ?assertEqual([1], all(1)),
  ?assertEqual([12, 21], lists:sort(all(2))),
  ?assertEqual([123, 132, 213, 231, 312, 321], lists:sort(all(3))).
