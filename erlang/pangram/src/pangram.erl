-module(pangram).

-export([is_pangram/1, test_version/0]).

%% API.

-spec is_pangram(string()) -> boolean().
is_pangram(Sentence) ->
    Alphabetic = [C || C <- string:to_lower(Sentence), is_alphabetic(C)],
    sets:size(sets:from_list(Alphabetic)) == alphabet_length().

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal.

alphabet_length() ->
    $z - $a + 1.

is_alphabetic(Character) when Character >= $a, Character =< $z ->
    true;
is_alphabetic(_Character) ->
    false.
