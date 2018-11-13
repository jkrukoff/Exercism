-module(poker).
-export([best/1]).

-type rank() :: 1..14.
-type suite() :: heart | diamond | spade | club.
-type classification() :: high_card | pair | two_pair | three_of_a_kind | straight | flush | full_house | four_of_a_kind | straight_flush.

-record(card, {rank :: rank(),
               suite :: suite()}).

-record(hand, {cards :: [#card{}],
               raw :: unicode:chardata()}).

-record(ranking, {hand :: #hand{},
                  classification :: classification(),
                  value :: [rank()]}).

%% API

-spec best([unicode:chardata()]) -> [unicode:chardata()].
best(Hands) ->
    io:format("Hands: ~w~n", [Hands]),
    Parsed = lists:flatmap(fun parse_hand/1, Hands),
    io:format("Parsed: ~w~n", [Parsed]),
    Ranked = lists:map(fun rank_hand/1, Parsed),
    io:format("Ranked: ~w~n", [Ranked]),
    Best = best_rankings(Ranked),
    io:format("Best: ~w~n", [Best]),
    % Must preserve original order. This also has the side effect of
    % normalizing the ace containing hands back into their canonical
    % form.
    [Hand || Hand <- Hands,
             lists:member(Hand, raw_rankings(Best))].

%% Internal

rank("2") -> [2];
rank("3") -> [3];
rank("4") -> [4];
rank("5") -> [5];
rank("6") -> [6];
rank("7") -> [7];
rank("8") -> [8];
rank("9") -> [9];
rank("10") -> [10];
rank("J") -> [11];
rank("Q") -> [12];
rank("K") -> [13];
rank("A") -> [1, 14].

ranks(Hand) ->
    [Card#card.rank || Card <- Hand#hand.cards].

suite("H") -> heart;
suite("D") -> diamond;
suite("S") -> spade;
suite("C") -> club.

suites(Hand) ->
    [Card#card.suite || Card <- Hand#hand.cards].

sort_cards(Cards) ->
    % Sort a list of cards by descending rank.
    lists:sort(
      fun(L, R) -> L#card.rank > R#card.rank end,
      Cards).

parse_hand(Hand) ->
    Cards = string:split(Hand, " ", all),
    lists:flatten(parse_cards(Hand, Cards, [])).

parse_cards(Hand, [], Acc) ->
    % Sort the cards by rank here during parsing, so that later steps
    % can make some simplifying assumptions without constantly
    % resorting.
    #hand{cards = sort_cards(Acc), raw = Hand};
parse_cards(Hand, [Card | Cards], Acc) ->
    Chars = string:to_graphemes(Card),
    {Rank, Suite} = lists:split(length(Chars) - 1, Chars),
    ParsedSuite = suite(Suite),

    % Deal with aces by returning multiple hands for each ace
    % encountered, one for each possible ranking.
    [parse_cards(
       Hand,
       Cards,
       [#card{rank = ParsedRank, suite = ParsedSuite} | Acc])
     || ParsedRank <- rank(Rank)].

rank_hand(Hand) ->
    IsFlush = is_flush(Hand),
    IsStraight = is_straight(Hand),
    {Duplicates, DuplicateValues} = count_ranks(Hand),

    % It is useful that the scoring order for hands also happens to
    % match the precendence order for determining ranking.
    {Classification, Value} = case {IsFlush, IsStraight, Duplicates} of
        {true, true, _} ->
            {straight_flush, ranks(Hand)};
        {_, _, [4 | _]} ->
            {four_of_a_kind, DuplicateValues};
        {_, _, [3, 2]} ->
            {full_house, DuplicateValues};
        {true, _, _} ->
            {flush, ranks(Hand)};
        {_, true, _} ->
            {straight, ranks(Hand)};
        {_, _, [3 | _]} ->
            {three_of_a_kind, DuplicateValues};
        {_, _, [2, 2 | _]} ->
            {two_pair, DuplicateValues};
        {_, _, [2 | _]} ->
            {pair, DuplicateValues};
        _ ->
            {high_card, ranks(Hand)}
    end,

    #ranking{hand = Hand,
             classification = Classification,
             value = Value}.

is_flush(Hand) ->
    [First | Suites] = suites(Hand),
    lists:all(fun (Suite) -> First == Suite end, Suites).

is_straight(Hand) ->
    [First | Ranks] = ranks(Hand),
    % Check if the rank of each card is one less than the previous.
    {Valid, _} = lists:foldl(
      fun (Elem, {Valid, Prev}) ->
          case {Valid, Prev - 1} of
              {false, _} ->
                  {false, 0};
              {true, Elem} ->
                  {true, Elem};
              _ ->
                  {false, 0}
          end
      end,
      {true, First},
      Ranks),
    Valid.

count_ranks(Hand) ->
    [First | Ranks] = ranks(Hand),
    Duplicates = lists:foldl(
      fun (Elem, [{Count, Rank} | Duplicates] = Acc) ->
        case Elem of
            Rank ->
                [{Count + 1, Rank} | Duplicates];
            _ ->
                [{1, Elem} | Acc]
        end
      end,
      [{1, First}],
      Ranks),

    % Return a list of duplicate counts useful for classification and
    % a corresponding list of ranks sorted by counts usable as a
    % secondary ranking.
    lists:unzip(lists:reverse(lists:sort(Duplicates))).

score_classification(high_card) -> 1;
score_classification(pair) -> 2;
score_classification(two_pair) -> 3;
score_classification(three_of_a_kind) -> 4;
score_classification(straight) -> 5;
score_classification(flush) -> 6;
score_classification(full_house) -> 7;
score_classification(four_of_a_kind) -> 8;
score_classification(straight_flush) -> 9.

score_ranking(Ranking) ->
    {score_classification(Ranking#ranking.classification),
     Ranking#ranking.value}.

best_rankings(Rankings) ->
    % Sort a list of rankings by descending score.
    [Best | Rest] = lists:sort(
      fun(L, R) -> score_ranking(L) > score_ranking(R) end,
      Rankings),
    % Return a list of tied rankings.
    [Best | [Ranking || Ranking <- Rest,
                        score_ranking(Best) == score_ranking(Ranking)]].

raw_rankings(Rankings) ->
    [Ranking#ranking.hand#hand.raw || Ranking <- Rankings].
