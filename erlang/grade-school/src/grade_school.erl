-module(grade_school).

-export([add/3,
         get/2,
         sort/1,
         new/0,
         test_version/0]).

-type grade() :: pos_integer().
-type name() :: unicode:chardata().

-record(school, {grades = #{} :: #{grade() => [name()]}}).
-type school() :: #school{}.

%% API

-spec new() -> school().
new() ->
    #school{}.

-spec add(name(), grade(), school()) -> school().
add(Name, Grade, #school{grades = Grades} = School) ->
    % This maintains an invariant that all names are sorted on insert,
    % so can avoid doing so on each retrieval.
    School#school{grades = maps:update_with(
        Grade,
        fun (Names) ->
            lists:sort([Name | Names])
        end,
        [Name],
        Grades)}.

-spec get(grade(), school()) -> [name()].
get(Grade, #school{grades = Grades}) ->
    maps:get(Grade, Grades, []).

-spec sort(school()) -> [{grade(), [name()]}].
sort(#school{grades = Grades}) ->
    lists:sort(maps:to_list(Grades)).

-spec test_version() -> integer().
test_version() ->
    1.
