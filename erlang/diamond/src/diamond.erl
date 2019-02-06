-module(diamond).

-type letter() :: $A..$Z.

-export([rows/1]).

%% API

-spec rows([letter()]) -> [[letter() | $ ], ...].
rows([Letter]) when Letter >= $A, Letter =< $Z ->
    rows($A, Letter, []).

%% Internal

rows(Letter, Final, Acc) when Letter > Final ->
    Rows = [HalfRow ++ tl(lists:reverse(HalfRow)) || HalfRow <- Acc],
    lists:reverse(Rows) ++ tl(Rows);
rows(Letter, Final, Acc) ->
    Outside = lists:duplicate(Final - Letter, $ ),
    Inside = lists:duplicate(Letter - $A, $ ),
    HalfRow = Outside ++ [Letter] ++ Inside,
    rows(Letter + 1, Final, [HalfRow | Acc]).
