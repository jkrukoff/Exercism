-module(bob).

-export([response/1, test_version/0]).

%% API

-spec response(unicode:chardata()) -> unicode:chardata().
response(String) ->
    IsQuestion = is_question(String),
    IsEmpty = is_empty(String),
    IsUppercase = is_uppercase(String),

    if
        IsEmpty ->
            % Not saying anything
            "Fine. Be that way!";
        IsQuestion andalso IsUppercase ->
            % Yelling a question.
            "Calm down, I know what I'm doing!";
        IsUppercase ->
            % Yelling something other than a question.
            "Whoa, chill out!";
        IsQuestion ->
            % Asking a question.
            "Sure.";
        true ->
            % Everything else.
            "Whatever."
    end.


-spec test_version() -> integer().
test_version() ->
    3.

%% Internal

is_question(String) ->
    Trimmed = string:trim(String, trailing),
    string:find(Trimmed, "?", trailing) == "?".

is_empty(String) ->
    string:trim(String) == "".

is_uppercase(String) ->
    string:uppercase(String) == String andalso
        string:lowercase(String) /= String.
