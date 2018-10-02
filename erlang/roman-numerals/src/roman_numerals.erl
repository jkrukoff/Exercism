-module(roman_numerals).

-export([numerals/1, test_version/0]).

%% API.

-spec numerals(integer()) -> string().
numerals(Number) when Number =< 4999 ->
    to_roman(digits(Number)).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

digits(Number) ->
    digits(Number, []).

% Number as list of digits, from most significant to least.
digits(0, Acc) ->
    Acc;
digits(Number, Acc) ->
    digits(Number div 10, [Number rem 10 | Acc]).

roman_digit(0, {_Ones, _Fives, _Tens}) ->
    [];
roman_digit(1, {Ones, _Fives, _Tens}) ->
    [Ones];
roman_digit(2, {Ones, _Fives, _Tens}) ->
    [Ones, Ones];
roman_digit(3, {Ones, _Fives, _Tens}) ->
    [Ones, Ones, Ones];
roman_digit(4, {Ones, Fives, _Tens}) ->
    [Ones, Fives];
roman_digit(5, {_Ones, Fives, _Tens}) ->
    [Fives];
roman_digit(6, {Ones, Fives, _Tens}) ->
    [Fives, Ones];
roman_digit(7, {Ones, Fives, _Tens}) ->
    [Fives, Ones, Ones];
roman_digit(8, {Ones, Fives, _Tens}) ->
    [Fives, Ones, Ones, Ones];
roman_digit(9, {Ones, _Fives, Tens}) ->
    [Ones, Tens].

% Number as iolist of roman digits, from most significant to least.
to_roman(Digits) ->
    RomanNumerals = [{"I", "V", "X"},
                     {"X", "L", "C"},
                     {"C", "D", "M"},
                     {"M", undefined, undefined}],
    lists:flatten(to_roman(lists:reverse(Digits), RomanNumerals, [])).

to_roman([], _Roman, Acc) ->
    Acc;
to_roman(_Digits, [], _Acc) ->
    throw({toolarge, "Number too large to convert."});
to_roman([Digit | NextDigit], [RomanDigits | NextRomanDigits], Acc) ->
    to_roman(NextDigit, NextRomanDigits, [roman_digit(Digit, RomanDigits) | Acc]).
