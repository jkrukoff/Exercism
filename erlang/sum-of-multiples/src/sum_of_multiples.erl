-module(sum_of_multiples).

-export([sumOfMultiples/2, test_version/0]).

%% API

-spec sumOfMultiples([pos_integer()], pos_integer()) -> pos_integer().
sumOfMultiples(Multiples, N) ->
    Candidates = lists:seq(1, N - 1),
    lists:sum([Candidate ||
               Candidate <- Candidates,
               is_multiple(Candidate, Multiples)]).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

is_multiple(Candidate, Multiples) ->
    lists:any(fun (Multiple) -> Candidate rem Multiple == 0 end, Multiples).
