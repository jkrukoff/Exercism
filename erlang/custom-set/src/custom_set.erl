-module(custom_set).

-export([add/2,
         contains/2,
         difference/2,
         disjoint/2,
         empty/1,
         equal/2,
         from_list/1,
         intersection/2,
         subset/2,
         union/2,
         test_version/0]).

% An unstated invariant of the tests is the sets must be directly
% comparable using ==, rather than using the equal/2 function.

-record(set, {items = #{} :: #{any() => true}}).
-opaque set() :: #set{}.
-export_type([set/0]).

%% API.

-spec add(any(), set()) -> set().
add(Elem, #set{items = Items}) ->
    #set{items = Items#{Elem => true}}.

-spec contains(any(), set()) -> boolean().
contains(Elem, #set{items = Items}) ->
    maps:get(Elem, Items, false).

% Return all items in Set1 that are not in Set2.
-spec difference(set(), set()) -> set().
difference(#set{items = Items1}, #set{items = Items2}) ->
    #set{items = maps:without(maps:keys(Items2), Items1)}.

% Do Set1 and Set2 not share any items?
-spec disjoint(set(), set()) -> boolean().
disjoint(Set1, Set2) ->
    Intersection = intersection(Set1, Set2),
    maps:size(Intersection#set.items) == 0.

-spec empty(set()) -> boolean().
empty(#set{items = Items}) ->
    maps:size(Items) == 0.

-spec equal(set(), set()) -> boolean().
equal(Set1, Set2) ->
    Set1 =:= Set2.

-spec from_list(list()) -> set().
from_list(List) ->
    #set{items = maps:from_list([{I, true} || I <- List])}.

% Return all items in both Set1 and Set2.
-spec intersection(set(), set()) -> set().
intersection(#set{items = Items1}, #set{items = Items2}) ->
    #set{items = maps:with(maps:keys(Items2), Items1)}.

% Are all items in Set1 contained in Set2?
-spec subset(set(), set()) -> boolean().
subset(Set1, #set{items = Items2} = Set2) ->
    Union = union(Set1, Set2),
    maps:size(Items2) == maps:size(Union#set.items).

% Return all items in either Set1 or Set2.
-spec union(set(), set()) -> set().
union(#set{items = Items1}, #set{items = Items2}) ->
    #set{items = maps:merge(Items1, Items2)}.

-spec test_version() -> integer().
test_version() ->
    2.
