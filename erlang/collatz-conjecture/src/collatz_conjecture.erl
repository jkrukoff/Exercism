-module(collatz_conjecture).

-export([steps/1,
         test_version/0]).

%% API

-spec steps(integer()) -> integer() | {error, string()}.
steps(N) when N > 0 ->
    steps(N, 0);
steps(_N) ->
    {error, "Only positive numbers are allowed"}.

-spec test_version() -> integer().
test_version() ->
    2.

%% Internal

steps(1, Count) ->
    Count;
steps(N, Count) when N rem 2 == 0 ->
    % When N is even.
    steps(N div 2, Count + 1);
steps(N, Count) ->
    % When N is odd.
    steps(3 * N + 1, Count + 1).
