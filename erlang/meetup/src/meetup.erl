-module(meetup).

-define(TEENTH_DAYS, [13, 14, 15, 16, 17, 18, 19]).

-export([schedule/4, test_version/0]).

-type day() :: monday | tuesday | wednesday | thursday | friday | saturday | sunday.
-type period() :: first | second | third | fourth | last | teenth.

%% API

-spec schedule(calendar:year(), calendar:month(), day(), period()) -> calendar:date().
schedule(Year, Month, DayOfWeek, Period) ->
    % Find the range of valid days in the given month.
    Start = date(Year, Month, 1),
    End = date(Year, Month, calendar:last_day_of_the_month(Year, Month)),
    % Find all days with the right day of week.
    CandidateDays = days(Start, End, DayOfWeek),
    % Filter to the one day that matches the given criteria.
    period(CandidateDays, Period).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

date(Year, Month, Day) ->
    Date = {Year, Month, Day},
    true = calendar:valid_date(Date),
    Date.

days({Year, Month, StartDay}, {Year, Month, EndDay}, DayOfWeek) ->
    WhichDay = day(DayOfWeek),
    [{Year, Month, Day} ||
     Day <- lists:seq(StartDay, EndDay),
     calendar:day_of_the_week(Year, Month, Day) == WhichDay].

day(monday) ->
    1;
day(tuesday) ->
    2;
day(wednesday) ->
    3;
day(thursday) ->
    4;
day(friday) ->
    5;
day(saturday) ->
    6;
day(sunday) ->
    7.

period(Days, first) ->
    lists:nth(1, Days);
period(Days, second) ->
    lists:nth(2, Days);
period(Days, third) ->
    lists:nth(3, Days);
period(Days, fourth) ->
    lists:nth(4, Days);
period(Days, last) ->
    lists:last(Days);
period(Days, teenth) ->
    {value, Date} = lists:search(fun is_teenth/1, Days),
    Date.

is_teenth({_Year, _Month, Day}) ->
    lists:member(Day, ?TEENTH_DAYS).
