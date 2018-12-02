-module(largest_series_product).

-export([lsp/2, test_version/0]).

%% API

-spec lsp(string(), integer()) -> integer() | error.
lsp(String, N) when N < 0; N > length(String) ->
    error;
lsp(_String, 0) ->
    1;
lsp(String, N) ->
    case to_integers(String) of
        {ok, Values} ->
            Sequences = sequences(Values, N),
            lists:max([multiply(S) || S <- Sequences]);
        {error, _} ->
            error
    end.

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

to_integers(String) ->
    to_integers(String, []).

to_integers([], Acc) ->
    {ok, Acc};
to_integers([Head | Tail], Acc) when Head >= $0, Head =< $9 ->
    to_integers(Tail, [list_to_integer([Head]) | Acc]);
to_integers(_String, _Acc) ->
    {error, invalid_character}.

sequences(Values, N) ->
    sequences(Values, N, []).

sequences(Values, N, Acc) when N > length(Values)->
    Acc;
sequences([_ | Tail] = Values, N, Acc) ->
    Sequence = lists:sublist(Values, N),
    sequences(Tail, N, [Sequence | Acc]).

multiply(Sequence) ->
    lists:foldl(fun erlang:'*'/2, 1, Sequence).
