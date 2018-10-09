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
    % Check the greedy path first.
    SortedCoins = lists:reverse(lists:sort(Coins)),
    % Bottom up caching of intermediate results.
    Solutions = lists:foldl(
        fun (A, Solutions) ->
            Solution = search(A, SortedCoins, Solutions, [], undefined),
            Solutions#{A => Solution}
        end,
        #{},
        lists:seq(1, Amount)),
    maps:get(Amount, Solutions).

%% Internal

search(0, _Coins, _Solutions, Solution, Best) ->
    fewest_coins(Solution, Best);
search(_Amount, [], _Solutions, _Solution, Best) ->
    Best;
search(Amount, _Coins, _Solutions, _Solution, Best) when Amount < 0 ->
    Best;
search(_Amount, _Coins, _Solutions, Solution, Best) when
        Best /= undefined,
        length(Solution) >= length(Best) ->
    Best;
search(Amount, [Coin | NextCoins] = Coins, Solutions, Solution, Best) ->
    case maps:get(Amount, Solutions, undefined) of
        undefined ->
            % Two recursive options to search, the set of coins
            % without the current coin, and the current set of coins
            % minus the value of the current coin.
            NextBest = search(Amount, NextCoins, Solutions, Solution, Best),
            search(Amount - Coin, Coins, Solutions, [Coin | Solution], NextBest);
        CachedBest ->
            fewest_coins(CachedBest ++ Solution, Best)
    end.

fewest_coins(S1, undefined) ->
    S1;
fewest_coins(S1, S2) when length(S1) =< length(S2) ->
    S1;
fewest_coins(_S1, S2) ->
    S2.
