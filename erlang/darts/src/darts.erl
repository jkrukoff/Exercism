-module(darts).

-type points() :: 10 | 5 | 1 | 0.

-export([score/2]).

%% API

-spec score(number(), number()) -> points().
score(X, Y) ->
    points(distance(X, Y)).

%% Internal

distance(X, Y) ->
    % By Pythagorean theorem.
    math:sqrt(X * X + Y * Y).

points(Distance) when Distance >= 0 ->
    if
        Distance =< 1 ->
            10;
        Distance =< 5 ->
            5;
        Distance =< 10 ->
            1;
        Distance > 10 ->
            0
    end.
