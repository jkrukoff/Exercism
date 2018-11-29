-module(word_count).

-export([count/1, test_version/0]).

%% API

-spec count(string()) -> dict:dict(string(), pos_integer()).
count(String) ->
    words(string:lowercase(String), dict:new()).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

words([], Words) ->
    Words;
words([C | _] = String, Words) when C >= $a, C =< $z; C >= $0, C =< $9 ->
    {Word, Rest} = string:take(String, word_chars()),
    words(Rest, dict:update_counter(Word, 1, Words));
words(String, Words) ->
    {_, Rest} = string:take(String, word_chars(), true),
    words(Rest, Words).

word_chars() ->
    lists:seq($a, $z) ++ lists:seq($0, $9).
