-module(grains).

-export([square/1, total/0, test_version/0]).

-spec square(pos_integer()) -> pos_integer().
square(N) when N > 0 ->
    % An integer equivalent of math:pow(2, N - 1), since erlang
    % doesn't support integer exponentiation.
    1 bsl (N - 1).

total(N) when N > 0 ->
    % Since each square is equal to setting a single bit in a binary
    % value, the total is equal to setting all of the bits in a binary
    % value.
    1 bsl N - 1.

-spec total() -> pos_integer().
total() ->
    total(64).

-spec test_version() -> integer().
test_version() ->
    1.
