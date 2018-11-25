-module(phone_number).

-define(PHONE_NUMBER_LENGTH, 10).

-export([number/1, areacode/1, pretty_print/1, test_version/0]).

-type digits() :: [$0..$9].

%% API

-spec number(string()) -> digits().
number(String) ->
    case digits(String) of
        [$1 | Digits] when length(Digits) == ?PHONE_NUMBER_LENGTH ->
            Digits;
        Digits when length(Digits) == ?PHONE_NUMBER_LENGTH ->
            Digits;
        _ ->
            lists:duplicate(?PHONE_NUMBER_LENGTH, $0)
    end.

-spec areacode(string()) -> digits().
areacode(String) ->
    {Area, _, _} = split(String),
    Area.

-spec pretty_print(string()) -> string().
pretty_print(String) ->
    {Area, First, Second} = split(String),
    lists:flatten(io_lib:format("(~s) ~s-~s", [Area, First, Second])).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

split(String) ->
    [D1, D2, D3, D4, D5, D6, D7, D8, D9, D10] = number(String),
    {[D1, D2, D3], [D4, D5, D6], [D7, D8, D9, D10]}.

digits(String) ->
    [C || C <- String, is_digit(C)].

is_digit(C) when C >= $0, C =< $9 ->
    true;
is_digit(_) ->
    false.
