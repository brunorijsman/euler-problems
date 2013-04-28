-module(euler57).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

series(0, Series) ->
    lists:reverse(Series);

series(More, Series) ->
    [{A, B} | _] = Series,
    NextA = A + 2*B,
    NextB = A + B,
    series(More - 1, [{NextA, NextB} | Series]).

more_digits({A, B}) ->
    number:nr_digits(A) > number:nr_digits(B).

solve(N) ->
    Series = series(N - 1, [{3, 2}]),
    FilteredSeries = lists:filter(fun more_digits/1, Series),
    length(FilteredSeries).
                                          
solve() ->
    solve(1000).

solve_test() ->
    ?assertEqual(153, solve()).

