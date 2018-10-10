-module(meetup).

-define(TEENTH_DAYS, [13, 14, 15, 16, 17, 18, 19]).

-export([schedule/4, test_version/0]).

-type day() :: monday | tuesday | wednesday | thursday | friday | saturday | sunday.
-type period() :: first | second | third | fourth | last | teenth.

%% API

-spec schedule(calendar:year(), calendar:month(), day(), period()) -> calendar:date().
schedule(Year, Month, DayOfWeek, Period) ->
    % Find all days in month with the right day of week.
    CandidateDays = candidate_days(Year, Month, DayOfWeek),
    % Filter to the one day that matches the given criteria.
    period(CandidateDays, Period).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal

candidate_days(Year, Month, DayOfWeek) ->
    DayNumber = day_number(DayOfWeek),
    [{Year, Month, Day} ||
     Day <- lists:seq(1, calendar:last_day_of_the_month(Year, Month)),
     calendar:day_of_the_week(Year, Month, Day) == DayNumber].

day_number(monday) ->
    1;
day_number(tuesday) ->
    2;
day_number(wednesday) ->
    3;
day_number(thursday) ->
    4;
day_number(friday) ->
    5;
day_number(saturday) ->
    6;
day_number(sunday) ->
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
