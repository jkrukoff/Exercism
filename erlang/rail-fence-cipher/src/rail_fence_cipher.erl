-module(rail_fence_cipher).

-export([encode/2,
         decode/2,
         test_version/0]).

%% API

-spec encode(string(), pos_integer()) -> string().
encode(Message, Rails) when Rails >= 2 ->
    Rows = [row(Row, Message, Rails) || Row <- lists:seq(0, Rails - 1)],
    lists:append(Rows).

-spec decode(string(), pos_integer()) -> string().
decode(Message, Rails) when Rails >= 2 ->
    plaintext(Message, Rails).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

enumerate(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).

cycle(Rails) ->
    % How many characters are in a single cycle?
    2 * (Rails - 1).

on_row(I, Rails) ->
    % Which row is a given plaintext index in?
    Cycle = cycle(Rails),
    Position = I rem Cycle,
    if
        Position < Rails ->
            Position;
        Position >= Rails ->
            Cycle - Position
    end.

row(Row, Message, Rails) ->
    [C || {I, C} <- enumerate(Message), on_row(I, Rails) == Row].

row_length(Row, Length, Rails) ->
    Cycle = cycle(Rails),
    Cycles = Length div Cycle,
    % Count how many characters of the final partial cycle are in the
    % given row.
    Extra = lists:sum([1 ||
                       I <- lists:seq(0, Length rem Cycle - 1),
                       on_row(I, Rails) == Row]),
    Extra + if
        Row == 0; Row == Rails - 1 ->
            % First and last row are only seen once per cycle.
            Cycles;
        Row > 0, Row < Rails - 1 ->
            % Everything else is seen twice per cycle.
            Cycles * 2
    end.

split(Message, Rails) ->
    Lengths = [row_length(Row, length(Message), Rails) ||
               Row <- lists:seq(0, Rails - 1)],
    split_lengths(Lengths, Message).

split_lengths([], []) ->
    [];
split_lengths([Length | Lengths], Message) ->
    {Row, Rest} = lists:split(Length, Message),
    [Row | split_lengths(Lengths, Rest)].

take(Row, Rows) ->
    % Take one character from the given row.
    {Before, [[C | Rest] | After]} = lists:split(Row, Rows),
    {C, Before ++ [Rest] ++ After}.

plaintext(Message, Rails) ->
    plaintext(0, length(Message), split(Message, Rails), Rails).

plaintext(I, Length, _Rows, _Rails) when I >= Length ->
    [];
plaintext(I, Length, Rows, Rails) ->
    {C, Rest} = take(on_row(I, Rails), Rows),
    [C | plaintext(I + 1, Length, Rest, Rails)].
