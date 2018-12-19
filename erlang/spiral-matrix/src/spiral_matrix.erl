-module(spiral_matrix).

-export([make/1,
         test_version/0]).

-record(remaining, {start_x :: non_neg_integer(),
                    end_x :: non_neg_integer(),
                    start_y :: non_neg_integer(),
                    end_y :: non_neg_integer()}).

-type matrix() :: [[pos_integer()]].

%% API

-spec make(non_neg_integer()) -> matrix().
make(0) ->
    [];
make(N) when N >= 1 ->
    Coordinates = coordinates(top, remaining(N)),
    to_matrix(N, Coordinates).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

remaining(N) ->
    End = N - 1,
    #remaining{start_x=0, end_x=End, start_y=0, end_y=End}.

coordinates(_, #remaining{start_x=StartX, end_x=EndX, start_y=StartY, end_y=EndY}) when
      StartX > EndX; StartY > EndY ->
    [];
coordinates(top, #remaining{start_x=StartX, end_x=EndX, start_y=StartY} = Remaining) ->
    Coordinates = [{X, StartY} || X <- lists:seq(StartX, EndX)],
    Coordinates ++ coordinates(right, Remaining#remaining{start_y=StartY + 1});
coordinates(right, #remaining{end_x=EndX, start_y=StartY, end_y=EndY} = Remaining) ->
    Coordinates = [{EndX, Y} || Y <- lists:seq(StartY, EndY)],
    Coordinates ++ coordinates(bottom, Remaining#remaining{end_x=EndX - 1});
coordinates(bottom, #remaining{start_x=StartX, end_x=EndX, end_y=EndY} = Remaining) ->
    Coordinates = [{X, EndY} || X <- lists:seq(EndX, StartX, -1)],
    Coordinates ++ coordinates(left, Remaining#remaining{end_y=EndY - 1});
coordinates(left, #remaining{start_x=StartX, start_y=StartY, end_y=EndY} = Remaining) ->
    Coordinates = [{StartX, Y} || Y <- lists:seq(EndY, StartY, -1)],
    Coordinates ++ coordinates(top, Remaining#remaining{start_x=StartX + 1}).

to_matrix(N, Coordinates) ->
    WithValues = enumerate(Coordinates),
    Compare = fun ({{X1, Y1}, _V1}, {{X2, Y2}, _V2}) ->
        {Y1, X1} =< {Y2, X2}
    end,
    values(split(N, lists:sort(Compare, WithValues))).

enumerate(L) ->
    lists:zip(L, lists:seq(1, length(L))).

split(_N, []) ->
    [];
split(N, Coordinates) ->
    {Row, Rest} = lists:split(N, Coordinates),
    [Row | split(N, Rest)].

values(Matrix) ->
    [[Value || {_Coordinate, Value} <- Row] || Row <- Matrix].
