-module(euler81common).

-export([get_test_square/0,
         read_square/2,
         get/3]).

-include_lib("eunit/include/eunit.hrl").

get_test_square() ->
    Values = [[131, 673, 234, 103, 18 ],
              [201, 96,  342, 965, 150],
              [630, 803, 746, 422, 111],
              [537, 699, 497, 121, 956],
              [805, 732, 524, 37,  331]],
    Size = 5,
    Square1 = dict:new(),
    Square2 = lists:foldl(fun(Y, Sq) -> set_test_row(Sq, Size, Values, Y) end, Square1, lists:seq(1, Size)),
    {Square2, Size}.

set_test_row(Square, Size, Values, Y) ->
    lists:foldl(fun(X, Sq) -> set_test_pos(Sq, Values, X, Y) end, Square, lists:seq(1, Size)).
    
set_test_pos(Square, Values, X, Y) ->
    Val = lists:nth(X, lists:nth(Y, Values)),
    dict:store({X, Y}, Val, Square).

read_square(FileName, Size) ->
    {ok, Fd} = file:open(FileName, [read]),
    Square = dict:new(),
    NewSquare = lists:foldl(fun(Y, Sq) -> read_row(Fd, Sq, Size, Y) end, Square, lists:seq(1, Size)),
    ok = file:close(Fd),
    NewSquare.

read_row(Fd, Square, Size, Y) ->
    lists:foldl(fun(X, Sq) -> read_pos(Fd, Sq, Size, X, Y) end, Square, lists:seq(1, Size)).

read_pos(Fd, Square, Size, X, Y) ->
    Format = case X of
                 Size -> "~d";
                 _    -> "~d,"
             end,
    {ok, [Nr]} = io:fread(Fd, "", Format),
    dict:store({X, Y}, Nr, Square).

get(Square, X, Y) ->
    dict:fetch({X, Y}, Square).
