-module(atbash_cipher).

-export([encode/1, decode/1, test_version/0]).

-define(CHUNK_SIZE, 5).

%% API

-spec encode(unicode:chardata()) -> unicode:chardata().
encode(String) ->
    Encrypted = encrypt(characters(String)),
    chunk(Encrypted).

-spec decode(unicode:chardata()) -> unicode:chardata().
decode(String) ->
    encrypt(characters(String)).

test_version() -> 1.

%% Internal

characters(String) ->
    string:to_graphemes(string:lowercase(String)).

encrypt([]) ->
    [];
encrypt([Letter | String]) when Letter >= $a, Letter =< $z ->
    Encrypted = $z - (Letter - $a),
    [Encrypted | encrypt(String)];
encrypt([Number | String]) when Number >= $0, Number =< $9 ->
    [Number | encrypt(String)];
encrypt([_ | String]) ->
    encrypt(String).

chunk(String) when length(String) > ?CHUNK_SIZE ->
    {Chunk, Tail} = lists:split(?CHUNK_SIZE, String),
    Chunk ++ " " ++ chunk(Tail);
chunk(String) ->
    String.
