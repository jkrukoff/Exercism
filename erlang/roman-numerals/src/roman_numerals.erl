-module(roman_numerals).

-export([numerals/1, test_version/0]).

%% API.

-spec numerals(integer()) -> string().
numerals(Number) when Number =< 4999 ->
    roman_digits(Number, roman_values(), []).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

roman_values() ->
    roman_values(1, "IVXLCDM", []).

% Not general purpose, sensitive to the length of the string of
% available digits.
roman_values(Value, [One, Five, Ten | Next], Acc) ->
    NextAcc = [{Value * 9, [One, Ten]},
               {Value * 5, [Five]},
               {Value * 4, [One, Five]},
               {Value, [One]} | Acc],
    roman_values(Value * 10, [Ten | Next], NextAcc);
roman_values(Value, [One | []], Acc) ->
    [{Value, [One]} | Acc].

% Given a list of digit values from largest to smallest, we construct
% an iolist of roman digits for each value smaller than the current
% number, then subtract the value of the digit added and continue.
roman_digits(0, _Values, Acc) ->
    lists:flatten(lists:reverse(Acc));
roman_digits(_Number, [], _Acc) ->
    throw({invalid_values, "Invalid roman values given, is there no 1 value?"});
roman_digits(Number, [{Value, _Digit} | NextValue], Acc) when Number - Value < 0 ->
    roman_digits(Number, NextValue, Acc);
roman_digits(Number, [{Value, Digit} | _] = Values, Acc) ->
    roman_digits(Number - Value, Values, [Digit | Acc]).
