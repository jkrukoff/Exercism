-module(nucleotide_count).

-type nucleotide() :: [$A | $T | $C | $G].

-export([count/2, nucleotide_counts/1, test_version/0]).

-spec count(nucleotide(), nucleotide()) -> pos_integer().
count(Dna, [N]) when N == $A; N == $T; N == $C; N == $G ->
    length([Found || Found <- Dna, Found == N]);
count(_Dna, _) ->
    % I don't understand why the test suite requires this to throw an
    % error. I'd have expected to use throw/1 instead, or let the
    % badmatch exception propagate.
    erlang:error("Invalid nucleotide").

-spec nucleotide_counts(nucleotide()) -> [{nucleotide(), pos_integer()}].
nucleotide_counts(Dna) ->
    [{[N], count(Dna, [N])} || N <- "ATCG"].

-spec test_version() -> integer().
test_version() ->
    1.
