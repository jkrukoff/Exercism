-module(rotational_cipher).

-define(ALPHA_LENGTH, ($z - $a + 1)).

-export([encrypt/2,
         decrypt/2,
         test_version/0]).

%% API

-spec encrypt(string(), non_neg_integer()) -> string().
encrypt(String, N) when N >= 0 ->
    [rotate_character(C, N) || C <- String].

-spec decrypt(string(), non_neg_integer()) -> string().
decrypt(String, N) when N >= 0 ->
    encrypt(String, -N rem ?ALPHA_LENGTH + ?ALPHA_LENGTH).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

rotate_character(C, Shift) when C >= $a, C =< $z ->
    rotate_character(C, Shift, $a);
rotate_character(C, Shift) when C >= $A, C =< $Z ->
    rotate_character(C, Shift, $A);
rotate_character(C, _Shift) ->
    C.

rotate_character(C, Shift, From) ->
    (C - From + Shift) rem ?ALPHA_LENGTH + From.
