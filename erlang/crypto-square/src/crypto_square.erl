-module(crypto_square).

-export([ciphertext/1]).

-type ciphertext() :: [$  | $0..$9 | $a..$z].

%% API

-spec ciphertext(string()) -> ciphertext().
ciphertext("") ->
    "";
ciphertext(Plaintext) ->
    Normalized = normalize(Plaintext),
    Square = split(Normalized, rows(length(Normalized))),
    lists:append(lists:join(" ", transpose(Square))).

%% Internal

is_alnum(C) when C >= $0, C =< $9; C >= $a, C =< $z ->
    true;
is_alnum(_C) ->
    false.

normalize(String) ->
    Lower = string:lowercase(String),
    [C || C <- Lower, is_alnum(C)].

rows(Length) ->
    ceil(math:sqrt(Length)).

split(Plaintext, Rows) when length(Plaintext) =< Rows ->
    [Plaintext ++ lists:duplicate(Rows - length(Plaintext), $ )];
split(Plaintext, Rows) ->
    {Row, Rest} = lists:split(Rows, Plaintext),
    [Row | split(Rest, Rows)].

is_empty([]) ->
    true;
is_empty(L) when is_list(L) ->
    false.

transpose(Matrix) ->
    Heads = [Head || [Head | _] <- Matrix],
    Tails = [Tail || [_ | Tail] <- Matrix],
    case lists:any(fun is_empty/1, Tails) of
        true ->
            [Heads];
        false ->
            [Heads | transpose(Tails)]
    end.
