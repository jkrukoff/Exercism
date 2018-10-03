-module(beer_song).

-export([verse/1,
         sing/1,
         sing/2,
         test_version/0]).

-spec verse(non_neg_integer()) -> string().
verse(0) ->
    io_lib:format(
        "No more bottles of beer on the wall, no more bottles of beer.~n"
        "Go to the store and buy some more, 99 bottles of beer on the wall.~n",
        []);
verse(1) ->
    io_lib:format(
       "1 bottle of beer on the wall, 1 bottle of beer.~n"
       "Take it down and pass it around, no more bottles of beer on the wall.~n",
       []);
verse(2) ->
    io_lib:format(
       "2 bottles of beer on the wall, 2 bottles of beer.~n"
       "Take one down and pass it around, 1 bottle of beer on the wall.~n",
       []);
verse(N) ->
    io_lib:format(
       "~b bottles of beer on the wall, ~b bottles of beer.~n"
       "Take one down and pass it around, ~b bottles of beer on the wall.~n",
       [N, N, N - 1]).

-spec sing(non_neg_integer()) -> string().
sing(N) ->
    sing(N, 0).

-spec sing(non_neg_integer(), non_neg_integer()) -> string().
sing(From, To) when From >= 0, To >= 0, From >= To ->
    lists:flatten(
        [[verse(N), io_lib:nl()] ||
         N <- lists:reverse(lists:seq(To, From))]).

-spec test_version() -> integer().
test_version() ->
    1.
