-module(parallel_letter_frequency).

-export([dict/1, frequency/1, test_version/0]).

-type frequencies() :: dict:dict(char(), pos_integer()).

%% API.

-spec dict([string()]) -> frequencies().
dict(Strings) ->
    sum(rpc:pmap({parallel_letter_frequency, frequency}, [], Strings)).

-spec frequency(string()) -> frequencies().
frequency(String) ->
    lists:foldl(
        fun (Character, Acc) ->
            dict:update_counter(Character, 1, Acc)
        end,
        dict:new(),
        String).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal.

sum(Frequencies) ->
    lists:foldl(
        fun (Dict, Acc) ->
            dict:merge(
                fun (_Key, Left, Right) ->
                    Left + Right
                end,
                Dict,
                Acc)
        end,
        dict:new(),
        Frequencies).
