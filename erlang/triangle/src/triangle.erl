-module(triangle).

-export([kind/3, test_version/0]).

-type category() :: equilateral | isosceles | scalene.

-spec kind(number(), number(), number()) -> category() | {error, string()}.
kind(A, B, C) when A =< 0; B =< 0; C =< 0 ->
    {error, "all side lengths must be positive"};
kind(A, B, C) when A + B =< C; B + C =< A; A + C =< B ->
    {error, "side lengths violate triangle inequality"};
kind(A, A, A) ->
    equilateral;
kind(A, B, C) when A == B; B == C; A == C ->
    isosceles;
kind(_A, _B, _C) ->
    scalene.

-spec test_version() -> integer().
test_version() ->
    1.
