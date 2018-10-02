-module(accumulate).

-export([accumulate/2, test_version/0]).

%% API.

-spec accumulate(fun((any()) -> any()), list()) -> list().
accumulate(Fn, Ls) ->
    accumulate(Fn, Ls, []).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal.

accumulate(_Fn, [], Acc) ->
    lists:reverse(Acc);
accumulate(Fn, [H | T], Acc) ->
    accumulate(Fn, T, [Fn(H) | Acc]).
