-module(gigasecond).

-export([from/1, test_version/0]).

-define(GIGASECOND, 1000000000).

%% API

-spec from(calendar:date() | calendar:datetime()) -> calendar:datetime().
from({{_, _, _}, {_, _, _}} = DateTime) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DateTime) + ?GIGASECOND);
from({_, _, _} = Date) ->
    from({Date, {0, 0, 0}}).

-spec test_version() -> integer().
test_version() ->
    1.
