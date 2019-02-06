-module(sublist).

-type relation() :: equal | sublist | superlist | unequal.

-export([is_equal/2,
         is_sublist/2,
         is_superlist/2,
         is_unequal/2,
         relation/2]).

%% API

-spec is_equal(list(), list()) -> boolean().
is_equal(L1, L2) ->
    relation(L1, L2) == equal.

-spec is_sublist(list(), list()) -> boolean().
is_sublist(L1, L2) ->
    case relation(L1, L2) of
        equal ->
            true;
        sublist ->
            true;
        _ ->
            false
    end.

-spec is_superlist(list(), list()) -> boolean().
is_superlist(L1, L2) ->
    case relation(L1, L2) of
        equal ->
            true;
        superlist ->
            true;
        _ ->
            false
    end.

-spec is_unequal(list(), list()) -> boolean().
is_unequal(L1, L2) ->
    relation(L1, L2) /= equal.

-spec relation(list(), list()) -> relation().
relation(L1, L2) ->
    case {contained(L1, L2), contained(L2, L1)} of
        {true, true} ->
            equal;
        {true, false} ->
            sublist;
        {false, true} ->
            superlist;
        {false, false} ->
            unequal
    end.

%% Internal

contained(L1, []) ->
    L1 == [];
contained(L1, [_ | Tail] = L2) ->
    case lists:prefix(L1, L2) of
        true ->
            true;
        false ->
            contained(L1, Tail)
    end.
