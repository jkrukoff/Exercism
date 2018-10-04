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
    Change = change(Amount, SortedCoins, []),
    ok = io:format("Change for ~p, ~p is ~p~n", [Amount, Coins, Change]),
    Change.

%% Internal

change(0, _Coins, Acc) ->
    Acc;
change(_Amount, [], _Acc) ->
    undefined;
change(Amount, [Coin | _] = Coins, Acc) when Amount - Coin >= Coin ->
    change(Amount - Coin, Coins, [Coin | Acc]);
change(Amount, [Coin | NextCoins], Acc) when Amount - Coin >= 0 ->
    change(Amount - Coin, NextCoins, [Coin | Acc]);
change(Amount, [Coin | NextCoins], Acc) when Amount - Coin < 0 ->
    change(Amount, NextCoins, Acc).
