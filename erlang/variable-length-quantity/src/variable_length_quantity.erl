-module(variable_length_quantity).

-define(MAX_CODED_BYTES, 5).

-export([encode/1,
         decode/1]).

%% API

encode(Integers) ->
    lists:append([encode_integer(I) || I <- Integers]).

decode([]) ->
    [];
decode(Integers) ->
    case decode_integer(Integers, []) of
        {ok, {Integer, Rest}} ->
            [Integer | decode(Rest)];
        {error, _ } ->
            undefined
    end.

%% Internal

take_bits(Integer) ->
    {Integer rem 128, Integer div 128}.

encode_integer(Integer) ->
    encode_integer(final, Integer).

encode_integer(final, Integer) ->
    {Digit, Rest} = take_bits(Integer),
    encode_integer(rest, Rest) ++ [Digit];
encode_integer(rest, 0) ->
    [];
encode_integer(rest, Integer) ->
    {Value, Rest} = take_bits(Integer),
    <<Digit:8>> = <<1:1, Value:7>>,
    encode_integer(rest, Rest) ++ [Digit].

decode_integer([], [_ | _]) ->
    {error, unterminated};
decode_integer([Byte | Rest], Partial) ->
    <<Continuation:1, Value:7>> = <<Byte>>,
    case Continuation of
        0 ->
            Values = lists:reverse([Value | Partial]),
            Padded = lists:duplicate(?MAX_CODED_BYTES - length(Values), 0) ++ Values,
            <<Integer:7/integer-unit:?MAX_CODED_BYTES>> = << <<V:7>> || V <- Padded >>,
            {ok, {Integer, Rest}};
        1 ->
            decode_integer(Rest, [Value | Partial])
    end.
