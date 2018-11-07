-module(scrabble_score).

-export([score/1, test_version/0]).

%% API

-spec score(string()) -> non_neg_integer().
score(Word) ->
    lists:sum([letter(L) || L <- string:uppercase(Word)]).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

letter($A) -> 1;
letter($E) -> 1;
letter($I) -> 1;
letter($O) -> 1;
letter($U) -> 1;
letter($L) -> 1;
letter($N) -> 1;
letter($R) -> 1;
letter($S) -> 1;
letter($T) -> 1;
letter($D) -> 2;
letter($G) -> 2;
letter($B) -> 3;
letter($C) -> 3;
letter($M) -> 3;
letter($P) -> 3;
letter($F) -> 4;
letter($H) -> 4;
letter($V) -> 4;
letter($W) -> 4;
letter($Y) -> 4;
letter($K) -> 5;
letter($J) -> 8;
letter($X) -> 8;
letter($Q) -> 10;
letter($Z) -> 10;
letter(_) -> 0.
