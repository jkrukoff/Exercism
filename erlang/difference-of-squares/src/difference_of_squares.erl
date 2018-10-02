-module(difference_of_squares).

-export([sum_of_squares/1,
         square_of_sum/1,
         difference_of_squares/1,
         test_version/0]).

%% API.

-spec sum_of_squares(pos_integer()) -> pos_integer().
sum_of_squares(N) when N >= 0 ->
    lists:sum([square(X) || X <- lists:seq(0, N)]).

-spec square_of_sum(pos_integer()) -> pos_integer().
square_of_sum(N) when N >= 0 ->
    square(lists:sum(lists:seq(0, N))).

-spec difference_of_squares(pos_integer()) -> pos_integer().
difference_of_squares(N) ->
    square_of_sum(N) - sum_of_squares(N).

-spec test_version() -> integer().
test_version() -> 1.

%% Internal.

square(N) ->
    N * N.
