-module(armstrong_numbers).

-export([is_armstrong_number/1]).

%% API

-spec is_armstrong_number(non_neg_integer()) -> boolean().
is_armstrong_number(N) when N >= 0 ->
    Digits = [list_to_integer([D]) || D <- integer_to_list(N)],
    lists:sum([round(math:pow(D, length(Digits))) || D <- Digits]) == N.
