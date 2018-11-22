-module(space_age).

-define(SECONDS_IN_MERCURY_YEAR, 7600543.81992).
-define(SECONDS_IN_VENUS_YEAR, 19414149.052176).
-define(SECONDS_IN_EARTH_YEAR, 31557600.0).
-define(SECONDS_IN_MARS_YEAR, 59354032.690079994).
-define(SECONDS_IN_JUPITER_YEAR, 374355659.124).
-define(SECONDS_IN_SATURN_YEAR, 929292362.8848).
-define(SECONDS_IN_URANUS_YEAR, 2651370019.3296).
-define(SECONDS_IN_NEPTUNE_YEAR, 5200418560.032001).

-export([ageOn/2, test_version/0]).

-type planet() :: mercury | venus | earth | mars | jupiter | saturn | uranus | neptune.

%% API

-spec ageOn(planet(), integer()) -> float().
ageOn(mercury, Seconds) ->
    Seconds / ?SECONDS_IN_MERCURY_YEAR;
ageOn(venus, Seconds) ->
    Seconds / ?SECONDS_IN_VENUS_YEAR;
ageOn(earth, Seconds) ->
    Seconds / ?SECONDS_IN_EARTH_YEAR;
ageOn(mars, Seconds) ->
    Seconds / ?SECONDS_IN_MARS_YEAR;
ageOn(jupiter, Seconds) ->
    Seconds / ?SECONDS_IN_JUPITER_YEAR;
ageOn(saturn, Seconds) ->
    Seconds / ?SECONDS_IN_SATURN_YEAR;
ageOn(uranus, Seconds) ->
    Seconds / ?SECONDS_IN_URANUS_YEAR;
ageOn(neptune, Seconds) ->
    Seconds / ?SECONDS_IN_NEPTUNE_YEAR.

-spec test_version() -> integer().
test_version() ->
    1.
