-module(euler54).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
    {ok, Fd} = file:open("../input/euler54.txt", [read]),
    Results = [match(Fd) || _Line <- lists:seq(1, 1000)],
    Count = length(lists:filter(fun(W) -> W == hand1 end, Results)),
    ok = file:close(Fd),
    Count.

match(Fd) ->
    Card11 = read_card(Fd, false),
    Card12 = read_card(Fd, false),
    Card13 = read_card(Fd, false),
    Card14 = read_card(Fd, false),
    Card15 = read_card(Fd, false),
    Card21 = read_card(Fd, false),
    Card22 = read_card(Fd, false),
    Card23 = read_card(Fd, false),
    Card24 = read_card(Fd, false),
    Card25 = read_card(Fd, true),
    Hand1 = [Card11, Card12, Card13, Card14, Card15],
    Hand2 = [Card21, Card22, Card23, Card24, Card25],
    poker:winner(Hand1, Hand2).

read_card(Fd, Last) ->
    {ok, [[C1], [C2]]} = io:fread(Fd, " ~n", "~c~c"),
    case Last of
        true  -> nop;
        false -> {ok, [[_Space]]} = io:fread(Fd, " ~n", "~c")
    end,
    CardStr = [C1, C2],
    poker:card_from_string(CardStr).

solve_test() ->
    ?assertEqual(376, solve()).

