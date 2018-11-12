-module(pascals_triangle).

-export([gen_pascals_triangle/1, test_version/0]).

%% API

-spec gen_pascals_triangle(integer()) -> [[pos_integer()]] | neg_integer().
gen_pascals_triangle(0) ->
    [];
gen_pascals_triangle(N) when N < 0 ->
    % I have no idea what this case is supposed to represent in the
    % test suite. I'd think that asking for negative rows should crash
    % or return an error.
    N;
gen_pascals_triangle(N) when N > 0 ->
    triangle(1, N, []).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

triangle(1, Limit, []) ->
    triangle(2, Limit, [[1]]);
triangle(N, Limit, Acc) when N > Limit ->
    lists:reverse(Acc);
triangle(N, Limit, [Last | _ ] = Acc) ->
    Next = sum_pairs(lists:flatten([0, Last, 0]), []),
    triangle(N + 1, Limit, [Next | Acc]).

sum_pairs([0], Acc) ->
    lists:reverse(Acc);
sum_pairs([Left, Right | Tail], Acc) ->
    sum_pairs([Right | Tail], [Left + Right | Acc]).
