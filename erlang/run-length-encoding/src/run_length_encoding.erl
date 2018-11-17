-module(run_length_encoding).

-export([encode/1, decode/1, test_version/0]).

-type data() :: [$A..$Z].
-type encoded() :: [$A..$Z | $0..$9].

%% API

-spec encode(data()) -> encoded().
encode([]) ->
    "";
encode([Head | Tail]) ->
    {Duplicates, String} = string:take(Tail, [Head]),
    case Duplicates of
        [] ->
            [];
        _ ->
            integer_to_list(length(Duplicates) + 1)
    end ++ [Head | encode(String)].

-spec decode(encoded()) -> data().
decode([]) ->
    "";
decode([Head | _] = String) when Head >= $0, Head =< $9 ->
    {Count, [Character | Tail]} = string:to_integer(String),
    lists:duplicate(Count, Character) ++ decode(Tail);
decode([Head | Tail]) ->
    [Head | decode(Tail)].

-spec test_version() -> integer().
test_version() ->
    1.
