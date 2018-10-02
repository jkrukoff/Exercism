-module(anagram).

-export([find/2, test_version/0]).

-spec find(string(), [string()]) -> [string()].
find(Word, Candidates) ->
    NormalizedWord = string:casefold(Word),
    SortedWord = lists:sort(NormalizedWord),

    IsAnagram = fun (Candidate) ->
        NormalizedCandidate = string:casefold(Candidate),
        NormalizedWord /= NormalizedCandidate andalso
            SortedWord == lists:sort(NormalizedCandidate)
    end,

    [Candidate || Candidate <- Candidates, IsAnagram(Candidate)].

-spec test_version() -> integer().
test_version() ->
    1.
