-module(protein_translation).

-export([proteins/1, test_version/0]).

-type nucleotides() :: [$A | $C | $G | $T].
-type proteins() :: [cysteine | leucine | methionine | phenylalanine | serine | tryptophan | tyrosine].

%% API

-spec proteins(nucleotides()) -> proteins().
proteins(Strand) ->
    proteins(Strand, []).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

proteins([], Acc) ->
    lists:reverse(Acc);
proteins([N1, N2, N3 | Strand], Acc) ->
    case protein([N1, N2, N3]) of
        stop ->
            proteins([], Acc);
        Protein ->
            proteins(Strand, [Protein | Acc])
    end.

protein("UGU") -> cysteine;
protein("UGC") -> cysteine;
protein("UUA") -> leucine;
protein("UUG") -> leucine;
protein("AUG") -> methionine;
protein("UUU") -> phenylalanine;
protein("UUC") -> phenylalanine;
protein("UCU") -> serine;
protein("UCC") -> serine;
protein("UCA") -> serine;
protein("UCG") -> serine;
protein("UGG") -> tryptophan;
protein("UAU") -> tyrosine;
protein("UAC") -> tyrosine;
protein("UAA") -> stop;
protein("UAG") -> stop;
protein("UGA") -> stop.
