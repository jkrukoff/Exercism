-module(strain).

-export([keep/2, discard/2, test_version/0]).

-type predicate() :: fun((any()) -> boolean).

%% API.

-spec keep(predicate(), list()) -> list().
keep(Fn, List) ->
    [Elem || Elem <- List, Fn(Elem)].

-spec discard(predicate(), list()) -> list().
discard(Fn, List) ->
    keep(fun (Elem) -> not Fn(Elem) end, List).

-spec test_version() -> integer().
test_version() ->
    1.
