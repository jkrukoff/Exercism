-module(series).

-export([from_string/2, test_version/0]).

%% API.

-spec from_string(pos_integer(), string()) -> [string()].
from_string(Width, String) when Width > 0 ->
    fanout(Width, String, []).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal.

fanout(Width, String, SubStrings) when length(String) < Width ->
    lists:reverse(SubStrings);
fanout(Width, [_ | NextString] = String, SubStrings) ->
    fanout(Width, NextString, [lists:sublist(String, Width) | SubStrings]).
