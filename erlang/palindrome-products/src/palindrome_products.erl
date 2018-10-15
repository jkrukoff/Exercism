-module(palindrome_products).

-export([smallest/2, largest/2, test_version/0]).

%% API

-spec smallest(integer(), integer()) -> {integer(), [{integer(), integer()}]} | {error, atom()}.
smallest(Min, Max) when Min > Max ->
    {error, invalid_range};
smallest(Min, Max) ->
    collect_on_condition(fun erlang:'<'/2, palindromes(Min, Max)).

-spec largest(integer(), integer()) -> {integer(), [{integer(), integer()}]} | {error, atom()}.
largest(Min, Max) when Min > Max ->
    {error, invalid_range};
largest(Min, Max) ->
    collect_on_condition(fun erlang:'>'/2, palindromes(Min, Max)).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

palindromes(Min, Max) ->
    Factors = lists:seq(Min, Max),
    [{F1 * F2, {F1, F2}} || F1 <- Factors, F2 <- Factors, F1 =< F2, is_palindrome(F1 * F2)].

is_palindrome(Product) ->
    % This turns out to be the expensive part of the inner loop.
    % Conversion to binary is a bit faster than conversion to a list,
    % and in combination with an early exit on the first and last
    % digit check nets a 3x speedup on my machine.
    BinString = integer_to_binary(Product),
    case binary:first(BinString) == binary:last(BinString) of
        true ->
            String = binary:bin_to_list(BinString),
            String == lists:reverse(String);
        false ->
            false
    end.

collect_on_condition(Condition, Palindromes) ->
    lists:foldl(
        fun ({Product, Factors}, undefined) ->
            {Product, [Factors]};
        ({Product, Factors}, {Product, AllFactors}) ->
            {Product, [Factors | AllFactors]};
        ({Product, Factors}, {Found, AllFactors}) ->
            case Condition(Product, Found) of
                true ->
                    {Product, [Factors]};
                false ->
                    {Found, AllFactors}
            end
        end,
        undefined,
        Palindromes).
