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
    SortedCoins = lists:reverse(lists:sort(Coins)),
    search(Amount, SortedCoins, [], undefined).

%% Internal

search(0, _Coins, Solution, Best) ->
    fewest_coins(Solution, Best);
search(_Amount, [], _Solution, Best) ->
    Best;
search(Amount, _Coins, _Solution, Best) when Amount < 0 ->
    Best;
search(_Amount, _Coins, Solution, Best) when
        Best /= undefined,
        length(Solution) >= length(Best) ->
    Best;
search(Amount, [Coin | NextCoins] = Coins, Solution, Best) ->
    NextBest = search(Amount, NextCoins, Solution, Best),
    fewest_coins(search(Amount - Coin, Coins, [Coin | Solution], Best), NextBest).

fewest_coins(S1, undefined) ->
    S1;
fewest_coins(undefined, S2) ->
    S2;
fewest_coins(S1, S2) when length(S1) =< length(S2) ->
    S1;
fewest_coins(_S1, S2) ->
    S2.
