-module(secret_handshake).

-export([commands/1, test_version/0]).

%% API

-spec commands(integer()) -> [string()].
commands(N) ->
    <<Reverse:1,
      Jump:1,
      Close:1,
      Blink:1,
      Wink:1>> = <<N:5>>,

    Flags = lists:zip([Wink, Blink, Close, Jump],
                      ["wink", "double blink", "close your eyes", "jump"]),
    Actions = [Action || {Flag, Action} <- Flags, Flag == 1],

    case Reverse == 1 of
        true ->
            lists:reverse(Actions);
        false ->
            Actions
    end.

-spec test_version() -> integer().
test_version() ->
    1.
