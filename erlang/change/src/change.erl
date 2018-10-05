-module(change).

-export([find_fewest_coins/2]).

%% API

-spec find_fewest_coins(integer(), [integer()]) -> [integer()] | undefined | {error, atom()}.
find_fewest_coins(0, _Coins) ->
    [];
find_fewest_coins(Amount, _Coins) when Amount < 0 ->
    {error, invalid_target_value};
find_fewest_coins(_Amount, []) ->
    {error, invalid_coins};
find_fewest_coins(Amount, Coins) ->
    Solutions = change(Amount, Coins),
    io:format("Solutions: ~p~n", [length(Solutions)]),
    lists:foldl(
        fun best_solution/2,
        undefined,
        Solutions).

%% Internal

change(Amount, _Coins) when Amount < 0 ->
    [invalid];
change(0, _Coins) ->
    [[]];
change(Amount, Coins) ->
    Solutions = [[C | S] || C <- Coins, S <- change(Amount - C, Coins), S /= invalid],
    Solutions.

best_solution(S1, undefined) ->
    S1;
best_solution(undefined, S2) ->
    S2;
best_solution(S1, S2) when length(S1) =< length(S2) ->
    S1;
best_solution(_S1, S2) ->
    S2.
