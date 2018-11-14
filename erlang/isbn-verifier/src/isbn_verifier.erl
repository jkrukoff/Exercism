-module(isbn_verifier).

-export([is_valid/1, test_version/0]).

%% API

-spec is_valid(string()) -> boolean().
is_valid(ISBN) ->
    case catch [value(I, E) || {I, E} <- enumerate(strip_all(ISBN, $-))] of
        invalid_index ->
            false;
        invalid_digit ->
            false;
        Digits ->
            lists:sum([I * E || {I, E} <- enumerate(lists:reverse(Digits))]) rem 11 == 0
    end.

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

strip_all(List, Remove) ->
    [E || E <- List, E /= Remove].

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

value(I, _E) when I > 10 -> throw(invalid_index);
value(_I, $0) -> 0;
value(_I, $1) -> 1;
value(_I, $2) -> 2;
value(_I, $3) -> 3;
value(_I, $4) -> 4;
value(_I, $5) -> 5;
value(_I, $6) -> 6;
value(_I, $7) -> 7;
value(_I, $8) -> 8;
value(_I, $9) -> 9;
value(10, $X) -> 10;
value(_I, _E) -> throw(invalid_digit).
