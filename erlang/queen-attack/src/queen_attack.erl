-module(queen_attack).

-type coordinate() :: {0..7, 0..7}.

-export([can_attack/2]).

%% API

-spec can_attack(coordinate(), coordinate()) -> boolean().
can_attack(WhiteQueen, BlackQueen) ->
    case slope(WhiteQueen, BlackQueen) of
        {0, 0} ->
            % Same space.
            throw(invalid_coordinates);
        {0, _} ->
            % Vertical.
            true;
        {_, 0} ->
            % Horizontal.
            true;
        {Y, X} when Y / X == 1; Y / X == -1 ->
            % Diagonal.
            true;
        {_, _} ->
            false
    end.

%% Internal

slope({X1, Y1}, {X2, Y2}) ->
    DeltaY = Y2 - Y1,
    DeltaX = X2 - X1,
    {DeltaY, DeltaX}.
