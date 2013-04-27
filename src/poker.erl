-module(poker).

-export([card_from_string/1,
         card_rank/1,
         card_suit/1,
         card_value/1,
         compare_card/2,
         hand_eval/1,
         winner/2]).

-include_lib("eunit/include/eunit.hrl").

card_from_string([C1, C2]) ->
    Rank = rank_from_char(C1),
    Suit = suit_from_char(C2),
    {Rank, Suit}.

rank_from_char($2) -> 2;
rank_from_char($3) -> 3;
rank_from_char($4) -> 4;
rank_from_char($5) -> 5;
rank_from_char($6) -> 6;
rank_from_char($7) -> 7;
rank_from_char($8) -> 8;
rank_from_char($9) -> 9;
rank_from_char($T) -> 10;
rank_from_char($J) -> jack;
rank_from_char($Q) -> queen;
rank_from_char($K) -> king;
rank_from_char($A) -> ace.
     
suit_from_char($H) -> hearts;
suit_from_char($D) -> diamonds;
suit_from_char($C) -> clubs;
suit_from_char($S) -> spades.
     
card_rank({Rank, _Suit}) -> Rank.

card_suit({_Rank, Suit}) -> Suit.

card_rank_value(ace)   -> 14;
card_rank_value(king)  -> 13;
card_rank_value(queen) -> 12;
card_rank_value(jack)  -> 11;
card_rank_value(Nr)    -> Nr.

hand_rank_value(straight_flush)  -> 9;
hand_rank_value(four_of_a_kind)  -> 8;
hand_rank_value(full_house)      -> 7;
hand_rank_value(flush)           -> 6;
hand_rank_value(straight)        -> 5;
hand_rank_value(three_of_a_kind) -> 4;
hand_rank_value(two_pair)        -> 3;
hand_rank_value(one_pair)        -> 2;
hand_rank_value(high_card)       -> 1.

card_value({Rank, _Suite}) -> card_rank_value(Rank).
        
compare_card(Card1, Card2) -> 
    CardValue1 = card_value(Card1),
    CardValue2 = card_value(Card2),
    CardSuit1 = card_suit(Card1),
    CardSuit2 = card_suit(Card2),
    {CardValue1, CardSuit1} >= {CardValue2, CardSuit2}.

canonize(Hand) -> lists:sort(fun compare_card/2, Hand).

%% match_xxx functions return false or {HandRank, [WinnerCardRank], [KickerCardRank]}
%% We useUsing the poker tie breaker rules described at http://www.limpinpoker.com/tyinghands.shtml
%% We treat a royal flush as a special case of a straight flush with an ace high

match_straight_flush(CanonicalHand, _RankCounts) ->
    case is_flush(CanonicalHand) andalso is_straight(CanonicalHand) of
        true  -> {straight_flush, card_ranks(CanonicalHand)};
        false -> false
    end.

match_four_of_a_kind(_CanonicalHand, RankCounts) ->
    [{Rank, Count} | _ ] = RankCounts,
    case Count of
        4 -> {four_of_a_kind, [Rank]};
        _ -> false
    end.

match_full_house(_CanonicalHand, RankCounts) ->
    [{Rank1, Count1}, {Rank2, Count2} | _ ] = RankCounts,
    case {Count1, Count2} of
        {3, 2} -> {full_house, [Rank1, Rank2]};
        _      -> false
    end.

match_flush(CanonicalHand, _RankCounts) ->
    case is_flush(CanonicalHand) of
        true  -> {flush, card_ranks(CanonicalHand)};
        false -> false
    end.

match_straight(CanonicalHand, _RankCounts) ->
    case is_straight(CanonicalHand) of
        true  -> {straight, card_ranks(CanonicalHand)};
        false -> false
    end.

match_three_of_a_kind(_CanonicalHand, RankCounts) ->
    [{Rank, Count} | _ ] = RankCounts,
    case Count of
        3 -> {three_of_a_kind, [Rank]};
        _ -> false
    end.

match_two_pair(_CanonicalHand, RankCounts) ->
    [{Rank1, Count1}, {Rank2, Count2} | _ ] = RankCounts,
    case {Count1, Count2} of
        {2, 2} -> {two_pair, [Rank1, Rank2]};
        _      -> false
    end.

match_one_pair(_CanonicalHand, RankCounts) ->
    [{Rank, Count} | _ ] = RankCounts,
    case Count of
        2 -> {one_pair, [Rank]};
        _ -> false
    end.

match_high_card(_CanonicalHand, RankCounts) ->
    [{Rank, _Count} | _ ] = RankCounts,
    {high_card, [Rank]}.

is_flush(CanonicalHand) ->
    [C1, C2, C3, C4, C5] = CanonicalHand,
    card_suit(C2) == card_suit(C1) andalso
        card_suit(C3) == card_suit(C2) andalso
        card_suit(C4) == card_suit(C3) andalso
        card_suit(C5) == card_suit(C4).

is_straight(CanonicalHand) ->
    [C1, C2, C3, C4, C5] = CanonicalHand,
    card_value(C2) == card_value(C1) - 1 andalso
        card_value(C3) == card_value(C2) - 1 andalso
        card_value(C4) == card_value(C3) - 1 andalso
        card_value(C5) == card_value(C4) - 1.

compare_rank_counts({Rank1, Count1}, {Rank2, Count2}) ->
    RankValue1 = card_rank_value(Rank1),
    RankValue2 = card_rank_value(Rank2),
    if
        Count1 > Count2         -> true;
        Count1 < Count2         -> false;
        RankValue1 > RankValue2 -> true;
        true                    -> false
    end.     

count_ranks(CardList) ->
    CountFun = fun(Card, Counts) -> dict:update_counter(card_rank(Card), 1, Counts) end,
    RankCountDict = lists:foldl(CountFun, dict:new(), CardList),
    RankCountList = dict:to_list(RankCountDict),
    lists:sort(fun compare_rank_counts/2, RankCountList).

card_ranks(Hand) ->
    lists:map(fun(Card) -> card_rank(Card) end, Hand).
    
kicker_cards(Hand, _WinnerCardRanks = []) ->
    Hand;

kicker_cards(Hand, _WinnerCardRanks = [Rank | Rest]) ->
    kicker_cards(lists:filter(fun(Card) -> card_rank(Card) /= Rank end, Hand), Rest).

hand_eval(Hand) ->
    CanonicalHand = canonize(Hand),
    RankCounts = count_ranks(CanonicalHand),
    {HandRank, WinnerCardRanks} = hand_eval(CanonicalHand, RankCounts, [fun match_straight_flush/2,
                                                                        fun match_four_of_a_kind/2,
                                                                        fun match_full_house/2,
                                                                        fun match_flush/2,
                                                                        fun match_straight/2,
                                                                        fun match_three_of_a_kind/2,
                                                                        fun match_two_pair/2,
                                                                        fun match_one_pair/2,
                                                                        fun match_high_card/2]),
    KickerCardRanks = card_ranks(kicker_cards(CanonicalHand, WinnerCardRanks)),
    {HandRank, WinnerCardRanks, KickerCardRanks}.

hand_eval(CanonizedHand, RankCounts, [MatchFun | Rest]) ->
    case MatchFun(CanonizedHand, RankCounts) of
        false -> hand_eval(CanonizedHand, RankCounts, Rest);
        Value -> Value
    end.

compare_ranks([], []) -> 
    tie;

compare_ranks([Rank1 | Rest1], [Rank2 | Rest2]) -> 
    Value1 = card_rank_value(Rank1),
    Value2 = card_rank_value(Rank2),
    if
        Value1 > Value2 -> hand1;
        Value1 < Value2 -> hand2;
        true            -> compare_ranks(Rest1, Rest2)
    end.

tie_breaker(Winners1, Kickers1, Winners2, Kickers2) ->
    case compare_ranks(Winners1, Winners2) of
        tie    -> compare_ranks(Kickers1, Kickers2);
        Winner -> Winner
    end.

winner(Hand1, Hand2) ->
    {Rank1, Winners1, Kickers1} = hand_eval(Hand1),
    {Rank2, Winners2, Kickers2} = hand_eval(Hand2),
    Value1 = hand_rank_value(Rank1),
    Value2 = hand_rank_value(Rank2),
    if
        Value1 > Value2 -> hand1;
        Value1 < Value2 -> hand2;
        true            -> tie_breaker(Winners1, Kickers1, Winners2, Kickers2)
    end.
                
hand_eval_test() ->
    RoyalFlush = [{queen, hearts}, {ace, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    ?assertEqual({straight_flush, [ace, king, queen, jack, 10], []}, hand_eval(RoyalFlush)),
    StraightFlush = [{queen, hearts}, {9, hearts}, {jack, hearts}, {10, hearts}, {8, hearts}],
    ?assertEqual({straight_flush, [queen, jack, 10, 9, 8], []}, hand_eval(StraightFlush)),
    FourOfAKind = [{3, hearts}, {4, spades}, {3, clubs}, {3, diamonds}, {3, spades}],
    ?assertEqual({four_of_a_kind, [3], [4]}, hand_eval(FourOfAKind)),
    FullHouse = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {3, hearts}],
    ?assertEqual({full_house, [3, queen], []}, hand_eval(FullHouse)),
    Flush = [{queen, hearts}, {3, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    ?assertEqual({flush, [king, queen, jack, 10, 3], []}, hand_eval(Flush)),
    Straight = [{6, hearts}, {4, diamonds}, {3, hearts}, {7, clubs}, {5, hearts}],
    ?assertEqual({straight, [7, 6, 5, 4, 3], []}, hand_eval(Straight)),
    ThreeOfAKind = [{3, hearts}, {4, spades}, {3, clubs}, {queen, diamonds}, {3, diamonds}],
    ?assertEqual({three_of_a_kind, [3], [queen, 4]}, hand_eval(ThreeOfAKind)),
    TwoPair = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    ?assertEqual({two_pair, [queen, 3], [6]}, hand_eval(TwoPair)),
    OnePair = [{4, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    ?assertEqual({one_pair, [queen], [6, 4, 3]}, hand_eval(OnePair)),
    HighCard = [{4, hearts}, {jack, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    ?assertEqual({high_card, [queen], [jack, 6, 4, 3]}, hand_eval(HighCard)).

winner_test() ->
    StraightFlush = [{queen, hearts}, {9, hearts}, {jack, hearts}, {10, hearts}, {8, hearts}],
    FourOfAKind = [{3, hearts}, {4, spades}, {3, clubs}, {3, diamonds}, {3, spades}],
    FullHouse = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {3, hearts}],
    Flush = [{queen, hearts}, {3, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    Straight = [{6, hearts}, {4, diamonds}, {3, hearts}, {7, clubs}, {5, hearts}],
    ThreeOfAKind = [{3, hearts}, {4, spades}, {3, clubs}, {queen, diamonds}, {3, diamonds}],
    TwoPair = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    OnePair = [{4, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    HighCard = [{4, hearts}, {jack, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    ?assertEqual(hand1, winner(StraightFlush, FourOfAKind)),
    ?assertEqual(hand2, winner(TwoPair, StraightFlush)),
    ?assertEqual(hand1, winner(FourOfAKind, FullHouse)),
    ?assertEqual(hand2, winner(OnePair, FourOfAKind)),
    ?assertEqual(hand1, winner(FullHouse, Flush)),
    ?assertEqual(hand2, winner(ThreeOfAKind, FullHouse)),
    ?assertEqual(hand1, winner(Flush, Straight)),
    ?assertEqual(hand2, winner(OnePair, Flush)),
    ?assertEqual(hand1, winner(Straight, ThreeOfAKind)),
    ?assertEqual(hand2, winner(OnePair, Straight)),
    ?assertEqual(hand1, winner(ThreeOfAKind, TwoPair)),
    ?assertEqual(hand2, winner(HighCard, ThreeOfAKind)),
    ?assertEqual(hand1, winner(TwoPair, OnePair)),
    ?assertEqual(hand2, winner(HighCard, TwoPair)),
    ?assertEqual(hand1, winner(OnePair, HighCard)),
    ?assertEqual(hand2, winner(HighCard, OnePair)).
    
straight_flush_tie_breaker_test() ->
    RoyalFlush = [{queen, hearts}, {ace, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    StraightFlush1 = [{queen, hearts}, {9, hearts}, {jack, hearts}, {10, hearts}, {8, hearts}],
    StraightFlush2 = [{7, hearts}, {9, hearts}, {jack, hearts}, {10, hearts}, {8, hearts}],
    StraightFlush3 = [{7, diamonds}, {9, diamonds}, {jack, diamonds}, {10, diamonds}, {8, diamonds}],
    ?assertEqual(hand1, winner(RoyalFlush, StraightFlush1)),
    ?assertEqual(hand2, winner(StraightFlush2, StraightFlush1)),
    ?assertEqual(tie, winner(StraightFlush2, StraightFlush3)).

four_of_a_kind_tie_breaker_test() ->
    FourOfAKind1 = [{3, hearts}, {4, spades}, {3, clubs}, {3, diamonds}, {3, spades}],
    FourOfAKind2 = [{queen, hearts}, {queen, spades}, {king, clubs}, {queen, diamonds}, {queen, hearts}],
    FourOfAKind3 = [{queen, hearts}, {queen, spades}, {ace, diamonds}, {queen, diamonds}, {queen, hearts}],
    FourOfAKind4 = [{queen, hearts}, {queen, spades}, {king, hearts}, {queen, diamonds}, {queen, hearts}],
    ?assertEqual(hand2, winner(FourOfAKind1, FourOfAKind2)),
    ?assertEqual(hand1, winner(FourOfAKind3, FourOfAKind2)),
    ?assertEqual(tie, winner(FourOfAKind4, FourOfAKind2)).
    
flush_tie_breaker_test() ->
    Flush1 = [{queen, hearts}, {3, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    Flush2 = [{queen, hearts}, {3, hearts}, {jack, hearts}, {4, hearts}, {10, hearts}],
    Flush3 = [{queen, hearts}, {2, hearts}, {jack, hearts}, {king, hearts}, {10, hearts}],
    Flush4 = [{queen, spades}, {3, spades}, {jack, spades}, {king, spades}, {10, spades}],
    ?assertEqual(hand1, winner(Flush1, Flush2)),
    ?assertEqual(hand1, winner(Flush1, Flush3)),
    ?assertEqual(tie, winner(Flush1, Flush4)).

straight_tie_breaker_test() ->
    Straight1 = [{6, hearts}, {4, diamonds}, {3, hearts}, {7, clubs}, {5, hearts}],
    Straight2 = [{6, hearts}, {4, diamonds}, {8, clubs}, {7, clubs}, {5, hearts}],
    Straight3 = [{6, clubs}, {4, diamonds}, {3, hearts}, {7, clubs}, {5, hearts}],
    ?assertEqual(hand2, winner(Straight1, Straight2)),
    ?assertEqual(tie, winner(Straight1, Straight3)).

three_of_a_kind_tie_breaker_test() ->
    ThreeOfAKind1 = [{3, hearts}, {4, spades}, {3, clubs}, {queen, diamonds}, {3, diamonds}],
    ThreeOfAKind2 = [{jack, hearts}, {4, spades}, {jack, clubs}, {queen, diamonds}, {jack, hearts}],
    ThreeOfAKind3 = [{3, hearts}, {5, spades}, {3, clubs}, {jack, diamonds}, {3, diamonds}],
    ThreeOfAKind4 = [{3, hearts}, {4, clubs}, {3, clubs}, {queen, hearts}, {3, clubs}],
    ?assertEqual(hand2, winner(ThreeOfAKind1, ThreeOfAKind2)),
    ?assertEqual(hand1, winner(ThreeOfAKind1, ThreeOfAKind3)),
    ?assertEqual(tie, winner(ThreeOfAKind1, ThreeOfAKind4)).

two_pair_tie_breaker_test() ->
    TwoPair1 = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    TwoPair2 = [{3, diamonds}, {jack, spades}, {3, clubs}, {jack, diamonds}, {6, hearts}],
    TwoPair3 = [{4, hearts}, {queen, spades}, {4, clubs}, {queen, diamonds}, {6, hearts}],
    TwoPair4 = [{3, diamonds}, {queen, spades}, {3, clubs}, {queen, diamonds}, {7, hearts}],
    TwoPair5 = [{3, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, diamonds}],
    ?assertEqual(hand1, winner(TwoPair1, TwoPair2)),
    ?assertEqual(hand2, winner(TwoPair1, TwoPair3)),
    ?assertEqual(hand2, winner(TwoPair1, TwoPair4)),
    ?assertEqual(tie, winner(TwoPair1, TwoPair5)).

one_pair_tie_breaker_test() ->
    OnePair1 = [{4, hearts}, {queen, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    OnePair2 = [{4, hearts}, {king, spades}, {3, clubs}, {king, hearts}, {6, hearts}],
    OnePair3 = [{4, hearts}, {queen, clubs}, {3, clubs}, {queen, diamonds}, {5, clubs}],
    OnePair4 = [{4, hearts}, {queen, spades}, {2, clubs}, {queen, clubs}, {6, hearts}],
    ?assertEqual(hand2, winner(OnePair1, OnePair2)),
    ?assertEqual(hand1, winner(OnePair1, OnePair3)),
    ?assertEqual(hand1, winner(OnePair1, OnePair4)).
    
high_card_tie_breaker_test() ->
    HighCard1 = [{4, hearts}, {jack, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    HighCard2 = [{4, hearts}, {jack, spades}, {3, diamonds}, {10, diamonds}, {6, hearts}],
    HighCard3 = [{2, hearts}, {jack, clubs}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    HighCard4 = [{4, diamonds}, {jack, spades}, {3, clubs}, {queen, diamonds}, {6, hearts}],
    ?assertEqual(hand1, winner(HighCard1, HighCard2)),
    ?assertEqual(hand1, winner(HighCard1, HighCard3)),
    ?assertEqual(tie, winner(HighCard1, HighCard4)).
    
card_from_string_test() ->
    ?assertEqual({2, hearts}, card_from_string("2H")),
    ?assertEqual({3, diamonds}, card_from_string("3D")),
    ?assertEqual({4, spades}, card_from_string("4S")),
    ?assertEqual({5, clubs}, card_from_string("5C")),
    ?assertEqual({6, hearts}, card_from_string("6H")),
    ?assertEqual({7, diamonds}, card_from_string("7D")),
    ?assertEqual({8, spades}, card_from_string("8S")),
    ?assertEqual({9, clubs}, card_from_string("9C")),
    ?assertEqual({10, hearts}, card_from_string("TH")),
    ?assertEqual({jack, diamonds}, card_from_string("JD")),
    ?assertEqual({queen, spades}, card_from_string("QS")),
    ?assertEqual({king, clubs}, card_from_string("KC")),
    ?assertEqual({ace, hearts}, card_from_string("AH")).

    
