-module(hamming).

-export([distance/2, test_version/0]).

-type nucleotides() :: [$A | $C | $G | $T].

%% API

-spec distance(nucleotides(), nucleotides()) -> non_neg_integer() | {error, string()}.
distance(Strand1, Strand2) when length(Strand1) /= length(Strand2) ->
    {error, "left and right strands must be of equal length"};
distance(Strand1, Strand2) ->
    lists:foldl(
      fun ({S1, S1}, Differences) ->
          Differences;
      (_, Differences) ->
          Differences + 1
      end,
      0,
      lists:zip(Strand1, Strand2)).

-spec test_version() -> integer().
test_version() ->
    2.
