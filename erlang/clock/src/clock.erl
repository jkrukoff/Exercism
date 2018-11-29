-module(clock).

-define(HOURS_IN_DAY, 24).
-define(MINUTES_IN_HOUR, 60).
-define(MINUTES_IN_DAY, (?HOURS_IN_DAY * ?MINUTES_IN_HOUR)).

-export([create/2, is_equal/2, minutes_add/2, to_string/1, test_version/0]).

-type hour() :: 0..(?HOURS_IN_DAY - 1).
-type minute() :: 0..(?MINUTES_IN_HOUR - 1).
-type time() :: 0..(?MINUTES_IN_DAY - 1).

-record(clock, {time = 0 :: time()}).
-type clock() :: #clock{}.

%% API

-spec create(hour(), minute()) -> clock().
create(Hour, Minute) ->
    #clock{time = normalize(minutes(Hour) + Minute)}.

-spec is_equal(clock(), clock()) -> boolean().
is_equal(ClockA, ClockB) ->
    ClockA == ClockB.

-spec minutes_add(clock(), integer()) -> clock().
minutes_add(#clock{time = Minutes}, AdditionalMinutes) ->
    #clock{time = normalize(Minutes + AdditionalMinutes)}.

-spec to_string(clock()) -> string().
to_string(#clock{time = Minutes}) ->
    io_lib:format("~2..0B:~2..0B", split(Minutes)).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

minutes(Hour) ->
    Hour * ?MINUTES_IN_HOUR.

split(Minutes) ->
    [Minutes div ?MINUTES_IN_HOUR, Minutes rem ?MINUTES_IN_HOUR].

normalize(Minutes) when Minutes < 0 ->
    Minutes rem ?MINUTES_IN_DAY + ?MINUTES_IN_DAY;
normalize(Minutes) ->
    Minutes rem ?MINUTES_IN_DAY.
