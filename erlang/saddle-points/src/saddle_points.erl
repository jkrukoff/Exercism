-module(saddle_points).

-export([saddle_points/1]).

%% API

-spec saddle_points([[integer()]]) -> [{non_neg_integer(), non_neg_integer()}].
saddle_points([[]]) ->
    [];
saddle_points(Matrix) ->
    Rows = [lists:max(Row) || Row <- Matrix],
    Columns = [lists:min(Column) || Column <- transpose(Matrix)],
    [{X, Y} || {X, Max} <- indexed(Rows), {Y, Min} <- indexed(Columns), Max == Min].

%% Internal

indexed(L) ->
    lists:zip(lists:seq(0, length(L) - 1), L).

is_empty([]) ->
    true;
is_empty(L) when is_list(L) ->
    false.

transpose(Matrix) ->
    Heads = [Head || [Head | _] <- Matrix],
    Tails = [Tail || [_ | Tail] <- Matrix],
    case lists:any(fun is_empty/1, Tails) of
        true ->
            [Heads];
        false ->
            [Heads | transpose(Tails)]
    end.
