%% Project Euler, problem 14
%% 
%% Find the longest sequence using a starting number under one million.

-module(euler14).

-export([solve/0, solve/1]).

-include_lib("eunit/include/eunit.hrl").

next_in_series(Nr) ->
    if
        Nr rem 2 == 0 ->
            Nr div 2;
        true ->
            3 * Nr + 1
    end.

series_length(StartNr) ->
    if
        StartNr == 1 ->
            1;
        true ->
            1 + series_length(next_in_series(StartNr))
    end.

longest_series(MaxStartNr) ->
    longest_series(1, MaxStartNr, 0, 0).

longest_series(StartNr, MaxStartNr, Longest, LongestStartNr) ->
    if
        StartNr >= MaxStartNr ->
            LongestStartNr;
        true ->
            Length = series_length(StartNr),
            if
                Length > Longest ->
                    longest_series(StartNr + 1, MaxStartNr, Length, StartNr);
                true ->
                    longest_series(StartNr + 1, MaxStartNr, Longest, LongestStartNr)
            end
    end.

solve() ->
    solve(1000000).

solve(N) ->
    longest_series(N).

solve_test() ->
    ?assertEqual(871, solve(1000)).
