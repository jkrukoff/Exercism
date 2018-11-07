-module(rna_transcription).

-export([to_rna/1, test_version/0]).

-type nucleotides() :: [$A | $T | $C | $G].

%% API

-spec to_rna(nucleotides()) -> nucleotides().
to_rna(Strand) ->
    [transcribe(N) || N <- Strand].

-spec test_version() -> integer().
test_version() ->
    3.

%% Internal

transcribe($G) -> $C;
transcribe($C) -> $G;
transcribe($T) -> $A;
transcribe($A) -> $U.
