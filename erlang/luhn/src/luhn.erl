-module(luhn).

-export([valid/1,
         test_version/0]).

%% API

-spec valid(string()) -> boolean().
valid(Number) ->
    valid(lists:reverse(Number), 1, 0).

-spec test_version() -> integer().
test_version() ->
    2.

%% Internal

valid([], Index, _Sum) when Index == 1; Index == 2 ->
    % Single and no digits are invalid.
    false;
valid([], _Index, Sum) ->
    % Valid if checksum passes.
    Sum rem 10 == 0;
valid([Digit | Number], Index, Sum) when Digit == $  ->
    valid(Number, Index, Sum);
valid([Digit | Number], Index, Sum) when Digit >= $0, Digit =< $9 ->
    valid(Number, Index + 1, Sum + double(Digit - $0, Index));
valid(_Number, _Index, _Sum) ->
    % Unexpected characters are invalid.
    false.

double(Digit, Index) when Index rem 2 == 0, Digit * 2 > 9 ->
    Digit * 2 - 9;
double(Digit, Index) when Index rem 2 == 0 ->
    Digit * 2;
double(Digit, _Index) ->
    Digit.
