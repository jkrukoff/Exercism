-module(minesweeper).

-export([annotate/1, test_version/0]).

-type minefield() :: [] | [[$  | $*]].
-type solution() :: [] | [[$  | $* | $1..$9]].

%% API

-spec annotate(minefield()) -> solution().
annotate([]) ->
    [];
annotate([[]]) ->
    [[]];
annotate(Minefield) ->
    Rows = [enumerate(R) || R <- Minefield],
    Coordinates = maps:from_list([{{X, Y}, Value} || {Y, Row} <- enumerate(Rows), {X, Value} <- Row]),
    Annotated = [{Y, X, annotate({X, Y}, Coordinates)} || {X, Y} <- maps:keys(Coordinates)],
    annotated_to_strings(Annotated).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

annotate(Coordinate, Coordinates) ->
    #{Coordinate := V} = Coordinates,
    case V of
        $* ->
            $*;
        $  ->
            case length(surrounding_mines(Coordinate, Coordinates)) of
                0 ->
                    $ ;
                Count ->
                    $0 + Count
            end
    end.

surrounding_mines({X, Y}, Coordinates) ->
    [$* ||
     AdjacentX <- [X - 1, X, X + 1],
     AdjacentY <- [Y - 1, Y, Y + 1],
     {AdjacentX, AdjacentY} /= {X, Y},
     maps:get({AdjacentX, AdjacentY}, Coordinates, $ ) == $*].

annotated_to_strings(Annotated) ->
    {_, Strings} = lists:foldl(
        fun ({Y, _X, Value}, {Y, [Row | Rows]}) ->
                {Y, [[Value | Row] | Rows]};
            ({Y, _X, Value}, {_, Rows}) ->
                {Y, [[Value] | Rows]}
        end,
        {undefined, []},
        lists:sort(Annotated)),
    lists:reverse([lists:reverse(S) || S <- Strings]).
