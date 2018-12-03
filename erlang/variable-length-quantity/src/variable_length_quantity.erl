-module(variable_length_quantity).

-export([encode/1,
         decode/1]).

-type bytes() :: [0..255].

%% API

-spec encode([integer()]) -> bytes().
encode(Integers) ->
    lists:append([encode_integer(I) || I <- Integers]).

-spec decode(bytes()) -> [integer()] | undefined.
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

code_length(Integer) ->
    % Calculate the number of 7 bit chunks needed to store the given
    % integer value.
    ceil(math:log2(Integer + 1) / 7).

encode_integer(0) ->
    [0];
encode_integer(Integer) ->
    Length = code_length(Integer),
    ContinuationBits = lists:duplicate(Length - 1, 1) ++ [0],
    ValueBits = [ V || <<V:7>> <= <<Integer:Length/unit:7>> ],
    [Byte ||
     {C, V} <- lists:zip(ContinuationBits, ValueBits),
     <<Byte>> <= <<C:1, V:7>>].

decode_integer([], [_ | _]) ->
    {error, unterminated};
decode_integer([Byte | Rest], Partial) ->
    <<Continuation:1, Value:7>> = <<Byte>>,
    case Continuation of
        0 ->
            Values = lists:reverse([Value | Partial]),
            Length = length(Values),
            <<Integer:Length/unit:7>> = << <<V:7>> || V <- Values >>,
            {ok, {Integer, Rest}};
        1 ->
            decode_integer(Rest, [Value | Partial])
    end.
