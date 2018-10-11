-module(minesweeper).

-define(MINE, $*).
-define(EMPTY, $ ).

-export([annotate/1, test_version/0]).

-type minefield() :: [] | [[?EMPTY | ?MINE]].
-type solution() :: [] | [[?EMPTY | ?MINE | $1..$9]].

%% API

-spec annotate(minefield()) -> solution().
annotate([]) ->
    [];
annotate([[]]) ->
    [[]];
annotate(Minefield) ->
    Coordinates = strings_to_coordinates(Minefield),
    Annotated = [{Y, X, annotate({X, Y}, Coordinates)} || {X, Y} <- maps:keys(Coordinates)],
    annotated_to_strings(Annotated).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

strings_to_coordinates(Strings) ->
    Rows = [enumerate(R) || R <- Strings],
    maps:from_list([{{X, Y}, Value} || {Y, Row} <- enumerate(Rows), {X, Value} <- Row]).

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

annotated_to_strings(Annotated) ->
    % First sort to put everything in order, then partition by Y
    % value.
    {_, Strings} = lists:foldl(
        fun ({Y, _X, Value}, {Y, [Row | Rows]}) ->
                {Y, [[Value | Row] | Rows]};
            ({Y, _X, Value}, {_, Rows}) ->
                {Y, [[Value] | Rows]}
        end,
        {undefined, []},
        lists:sort(Annotated)),
    % But we put it together backwards, so reverse all the things.
    lists:reverse([lists:reverse(S) || S <- Strings]).

annotate(Coordinate, Coordinates) ->
    #{Coordinate := V} = Coordinates,
    case V of
        ?MINE ->
            ?MINE;
        ?EMPTY ->
            case surrounding_mines(Coordinate, Coordinates) of
                0 ->
                    ?EMPTY;
                Count ->
                    $0 + Count
            end
    end.

surrounding_mines({X, Y}, Coordinates) ->
    length([?MINE ||
            AdjacentX <- [X - 1, X, X + 1],
            AdjacentY <- [Y - 1, Y, Y + 1],
            {AdjacentX, AdjacentY} /= {X, Y},
            maps:get({AdjacentX, AdjacentY}, Coordinates, ?EMPTY) == ?MINE]).
