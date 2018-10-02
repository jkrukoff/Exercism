-module(bracket_push).

-export([is_paired/1]).

-spec is_paired(string()) -> boolean().
is_paired(String) ->
    is_paired(String, []).

is_paired([], []) ->
    % End of string and all pairs matched.
    true;
is_paired([], _Stack) ->
    % End of string and not all pairs were matched.
    false;
is_paired([Bracket | String], Stack) when Bracket == $(; Bracket == ${; Bracket == $[ ->
    % Opening bracket, append to stack and continue.
    is_paired(String, [Bracket | Stack]);
is_paired([$) | String], [$( | Stack]) ->
    % Closing bracket with match on the stack.
    is_paired(String, Stack);
is_paired([$} | String], [${ | Stack]) ->
    is_paired(String, Stack);
is_paired([$] | String], [$[ | Stack]) ->
    is_paired(String, Stack);
is_paired([Bracket | _String], _Stack) when Bracket == $); Bracket == $}; Bracket == $] ->
    % Closing bracket, but there was no match on the stack.
    false;
is_paired([_NotBracket | String], Stack) ->
    % Not a bracket, continue.
    is_paired(String, Stack).
