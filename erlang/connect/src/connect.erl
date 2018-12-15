-module(connect).

-include_lib("stdlib/include/assert.hrl").

-define(GOAL_START_X, start_x).
-define(GOAL_END_X, end_x).
-define(GOAL_START_O, start_o).
-define(GOAL_END_O, end_o).

-export([winner/1]).

-type board() :: [$X | $O | $. | $ , ...].

%% API

-spec winner([board()]) -> x | o | undefined.
winner(Board) ->
    Parsed = parse_board(Board),
    case {is_winner(Parsed, x), is_winner(Parsed, o)} of
        {true, true} ->
            % This should never be possible unless there's a bug in
            % the library.
            throw(impossible_board);
        {true, false} ->
            x;
        {false, true} ->
            o;
        {false, false} ->
            undefined
    end.

%% Internal

parse($X) -> x;
parse($O) -> o;
parse($.) -> undefined;
parse($ ) -> space.

parse_board([_ | _] = Board) ->
    Parsed = [[parse(C) || C <- Row, parse(C) /= space] || Row <- Board],
    Width = length(hd(Parsed)),
    ok = ?assert(lists:all(fun (Row) -> Width == length(Row) end, Parsed),
                 "All rows should be of identical length."),
    Parsed.

graph_board(G, Board) ->
    G = add_vertices(G, Board),
    ok.

dimensions([Row | _] = Board) ->
    Width = length(Row),
    Height = length(Board),
    {Width, Height}.

add_vertices(G, Board) ->
    Dimensions = dimensions(Board),
    ok = add_vertices(G, Board, {0, 0}, Dimensions),
    ok = add_goals(G, Dimensions),
    G.

add_undirected_edge(G, V1, V2, Label) ->
    _ = digraph:add_edge(G, V1, V2, Label),
    _ = digraph:add_edge(G, V2, V1, Label),
    ok.

add_w_edge(G, V, {X, Y}) when X > 0 ->
    add_undirected_edge(G, V, {X - 1, Y}, west);
add_w_edge(_G, _V1, _V2) ->
    ok.

add_nw_edge(G, V, {X, Y}) when Y > 0 ->
    add_undirected_edge(G, V, {X, Y - 1}, northwest);
add_nw_edge(_G, _V1, _V2) ->
    ok.

add_ne_edge(G, V, {X, Y}, {Width, _Height}) when Y > 0, X < Width - 1 ->
    add_undirected_edge(G, V, {X + 1, Y - 1}, northeast);
add_ne_edge(_G, _V1, _V2, _Dimensions) ->
    ok.

add_edges(G, V, Coordinates, Dimensions) ->
    ok = add_w_edge(G, V, Coordinates),
    ok = add_nw_edge(G, V, Coordinates),
    ok = add_ne_edge(G, V, Coordinates, Dimensions).

add_vertices(_G, [], {_X, _Y}, _Dimensions) ->
    ok;
add_vertices(G, [[] | Board], {_X, Y}, Dimensions) ->
    add_vertices(G, Board, {0, Y + 1}, Dimensions);
add_vertices(G, [[Space | Row] | Board], {X, Y}, Dimensions) ->
    V = digraph:add_vertex(G, {X, Y}, Space),
    ok = add_edges(G, V, {X, Y}, Dimensions),
    add_vertices(G, [Row | Board], {X + 1, Y}, Dimensions).

add_goals(G, {Width, Height}) ->
    StartX = digraph:add_vertex(G, ?GOAL_START_X, x),
    ok = lists:foreach(fun ({X, Y}) -> digraph:add_edge(G, StartX, {X, Y}) end,
                       [{0, Y} || Y <- lists:seq(0, Height)]),
    StartO = digraph:add_vertex(G, ?GOAL_START_O, o),
    ok = lists:foreach(fun ({X, Y}) -> digraph:add_edge(G, StartO, {X, Y}) end,
                       [{X, 0} || X <- lists:seq(0, Width)]),
    EndX = digraph:add_vertex(G, ?GOAL_END_X, x),
    ok = lists:foreach(fun ({X, Y}) -> digraph:add_edge(G, {X, Y}, EndX) end,
                       [{Width - 1, Y} || Y <- lists:seq(0, Height)]),
    EndO = digraph:add_vertex(G, ?GOAL_END_O, o),
    ok = lists:foreach(fun ({X, Y}) -> digraph:add_edge(G, {X, Y}, EndO) end,
                       [{X, Height - 1} || X <- lists:seq(0, Width)]).

filter_board(G, Keep) ->
    true = digraph:del_vertices(G, [V ||
                                    Vertex <- digraph:vertices(G),
                                    {V, Label} <- [digraph:vertex(G, Vertex)],
                                    Label /= Keep]),
    ok.

is_winner(Parsed, x) ->
    is_winner(Parsed, x, ?GOAL_START_X, ?GOAL_END_X);
is_winner(Parsed, o) ->
    is_winner(Parsed, o, ?GOAL_START_O, ?GOAL_END_O).

is_winner(Parsed, Side, Start, End) ->
    G = digraph:new([private]),
    try
        % It'd have been nice if there was a way to copy a board, so I
        % could re-use this base state for X and O.
        ok = graph_board(G, Parsed),
        % It turns out the hardest part of this for me to get right
        % was the edge contstruction. As such, it made sense to me to
        % construct the board the same way every time, then filter to
        % only the interesting vertices instead of trying to only
        % build edges for the currently interesting side.
        ok = filter_board(G, Side),
        % Creating a custom search function that knew how to respect
        % the vertex labels to reject invalid paths would allow for
        % easily reusing the same graph. That's probably what I'd do
        % in a version of this that tried to implement more of the
        % games rules.
        Path = digraph:get_path(G, Start, End),
        % Helpful debugging:
        % io:format("Vertices: ~w~n", [digraph:vertices(G)]),
        % io:format("Edges: ~w~n", [digraph:edges(G)]),
        % io:format("Path: ~w~n", [Path]),
        % case Path of
        %     false ->
        %         ok;
        %     Path when is_list(Path) ->
        %         io:format("Neighbours: ~w~n", [[digraph:out_neighbours(G, V) || V <- Path]])
        % end,
        Path /= false
    after
        true = digraph:delete(G)
    end.
