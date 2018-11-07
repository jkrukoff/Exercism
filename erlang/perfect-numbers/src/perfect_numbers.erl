-module(perfect_numbers).

-export([classify/1, test_version/0]).

-type classification() :: perfect | abundant | deficient.

%% API

-spec classify(integer()) -> classification() | {error, string()}.
classify(N) when N =< 0 ->
    {error, "Classification is only possible for natural numbers."};
classify(N) ->
    Sum = lists:sum(factors(N)),
    if
        Sum < N ->
            deficient;
        Sum == N ->
            perfect;
        Sum > N ->
            abundant
    end.

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

factors(N) ->
    [F || F <- lists:seq(1, N div 2), N rem F == 0].
