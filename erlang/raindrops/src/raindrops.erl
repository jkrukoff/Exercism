-module(raindrops).

-define(FACTORS, [3, 5, 7]).

-export([convert/1, test_version/0]).

%% API

-spec convert(integer()) -> string().
convert(Number) ->
    case [sound(F) || F <- ?FACTORS, Number rem F == 0] of
        [] ->
            integer_to_list(Number);
        Sounds ->
            lists:flatten(Sounds)
    end.

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

sound(3) ->
    "Pling";
sound(5) ->
    "Plang";
sound(7) ->
    "Plong".
