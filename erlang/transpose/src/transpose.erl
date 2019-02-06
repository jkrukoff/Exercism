-module(transpose).

-export([transpose/1]).

%% API

-spec transpose([string()]) -> [string()].
transpose([]) ->
    [];
transpose(Lines) ->
    Heads = [Head || [Head | _] <- Lines],
    Tails = [Tail || [_ | Tail] <- Lines],
    case lists:all(fun is_empty/1, Tails) of
        true ->
            [Heads];
        false ->
            [Heads | transpose(pad(Tails))]
    end.

%% Internal

is_empty([]) ->
    true;
is_empty(L) when is_list(L) ->
    false.

% Find all empty tails before the last non-empty tail and replace with
% a space.
pad(Tails) ->
    {Empty, Pad} = lists:splitwith(fun is_empty/1, lists:reverse(Tails)),
    lists:reverse(Empty ++ lists:map(fun ([]) -> " "; (T) -> T end, Pad)).
