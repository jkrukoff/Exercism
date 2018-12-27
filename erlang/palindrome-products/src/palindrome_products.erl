-module(palindrome_products).

-export([smallest/2, largest/2, test_version/0]).

%% API

-spec smallest(integer(), integer()) -> {integer(), [{integer(), integer()}]} | {error, atom()}.
smallest(Min, Max) when Min > Max ->
    {error, invalid_range};
smallest(Min, Max) ->
    Factors = lists:seq(Min, Max),
    palindromes(Factors, Factors, fun erlang:'<'/2, undefined).

-spec largest(integer(), integer()) -> {integer(), [{integer(), integer()}]} | {error, atom()}.
largest(Min, Max) when Min > Max ->
    {error, invalid_range};
largest(Min, Max) ->
    Factors = lists:seq(Max, Min, -1),
    palindromes(Factors, Factors, fun erlang:'>'/2, undefined).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

palindromes([], _F2, _Condition, Best) ->
    Best;
palindromes([_ | F1Tail], [], Condition, Best) ->
    palindromes(F1Tail, F1Tail, Condition, Best);
palindromes([F1Head | _] = F1, [F2Head | F2Tail], Condition, Best) ->
    EvenBetter = case is_best(Condition, F1Head * F2Head, Best) of
        true ->
            {F1Head * F2Head, [{F1Head, F2Head}]};
        equal ->
            {Product, Factors} = Best,
            {Product, [{F1Head, F2Head} | Factors]};
        false ->
            Best
    end,
    palindromes(F1, F2Tail, Condition, EvenBetter).

% Order matters for performance here, since we've got quite a few
% possibilities to check. As is_palindrome is the most expensive of
% our checks, attempt to delay that until we're sure it's necessary.
is_best(_Condition, Product, undefined) ->
    is_palindrome(Product);
is_best(_Condition, Product, {Product, _Factors}) ->
    equal;
is_best(Condition, Product, {Best, _Factors}) ->
    Condition(Product, Best) andalso is_palindrome(Product).

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
