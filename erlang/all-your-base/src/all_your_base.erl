-module(all_your_base).

-export([convert/3, test_version/0]).

%% API

-spec convert([integer()], integer(), integer()) -> {ok, [non_neg_integer()]} | {error, atom()}.
convert(_Digits, SrcBase, _DstBase) when SrcBase =< 1 ->
    {error, invalid_src_base};
convert(_Digits, _SrcBase, DstBase) when DstBase =< 1 ->
    {error, invalid_dst_base};
convert(Digits, SrcBase, DstBase) ->
    case from_base(Digits, SrcBase) of
        {error, _} = Error ->
            Error;
        {ok, Value} ->
            {ok, to_base(Value, DstBase)}
    end.

-spec test_version() -> integer().
test_version() -> 1.

%% Internal

from_base(Digits, SrcBase) ->
    from_base(lists:reverse(Digits), SrcBase, 1, 0).

from_base([], _SrcBase, _Power, Acc) ->
    {ok, Acc};
from_base([Digit | _Digits], _SrcBase, _Power, _Acc) when Digit < 0 ->
    {error, negative};
from_base([Digit | _Digits], SrcBase, _Power, _Acc) when Digit >= SrcBase ->
    {error, not_in_base};
from_base([Digit | Digits], SrcBase, Power, Acc) ->
    Value = Digit * Power,
    from_base(Digits, SrcBase, Power * SrcBase, Acc + Value).

to_base(Value, DstBase) ->
    to_base(Value, DstBase, []).

to_base(0, _DstBase, Acc) ->
    Acc;
to_base(Value, DstBase, Acc) ->
    Digit = Value rem DstBase,
    to_base(Value div DstBase, DstBase, [Digit | Acc]).
