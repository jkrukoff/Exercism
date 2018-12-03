-module(etl).

-export([transform/1, test_version/0]).

-spec transform([{pos_integer(), [string()]}]) -> [{string(), pos_integer()}].
transform(Data) ->
    [{string:lowercase(Letter), Score} || {Score, Letters} <- Data, Letter <- Letters].

-spec test_version() -> integer().
test_version() ->
    1.
