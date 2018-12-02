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

encode_integer(0) ->
    [0];
encode_integer(Integer) ->
    ContinuationBits = lists:duplicate(?MAX_CODED_BYTES - 1, 1) ++ [0],
    ValueBits = [ I || <<I:7/integer>> <= <<Integer:7/integer-unit:?MAX_CODED_BYTES>> ],
    Parts = lists:dropwhile(fun ({_, V}) -> V == 0 end,
                            lists:zip(ContinuationBits, ValueBits)),
    Bytes = << <<Continuation:1/integer, Value:7/integer>> ||
               {Continuation, Value} <- Parts >>,
    binary_to_list(Bytes).

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
