-module(leap).

-export([leap_year/1, test_version/0]).

-spec leap_year(integer()) -> boolean().
leap_year(Year) when Year rem 4 == 0, Year rem 100 /= 0; Year rem 400 == 0 ->
    true;
leap_year(_Year) ->
    false.

-spec test_version() -> integer().
test_version() ->
    4.
