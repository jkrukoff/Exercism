-module(isogram).

-export([is_isogram/1, test_version/0]).

% API

-spec is_isogram(string()) -> boolean().
is_isogram(String) ->
    Normalized = string:casefold(String),
    is_isogram(Normalized, sets:new()).

-spec test_version() -> integer().
test_version() ->
    1.

% Internal

is_isogram([], _Seen) ->
    % If we've searched all the non-ignored characters and not found a
    % duplicate, it's an isogram.
    true;
is_isogram([Char | String], Seen) ->
    case {is_ignored(Char), sets:is_element(Char, Seen)} of
        {true, _} ->
            is_isogram(String, Seen);
        {false, true} ->
            % If we find a duplicate non-ignored character we know
            % it's not an isogram, even though we haven't finished
            % yet.
            false;
        {false, false} ->
            is_isogram(String, sets:add_element(Char, Seen))
    end.

is_ignored($ ) ->
    true;
is_ignored($-) ->
    true;
is_ignored(_) ->
    false.
