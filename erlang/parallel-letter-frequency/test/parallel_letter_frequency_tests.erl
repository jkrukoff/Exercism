-module( parallel_letter_frequency_tests ).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").

single_test() ->
  Frequencies = dict:to_list( parallel_letter_frequency:dict(["asd"]) ),
  ?assert( lists:sort(Frequencies) =:= [{$a,1},{$d,1},{$s,1}] ).

double_test() ->
  Frequencies = dict:to_list( parallel_letter_frequency:dict(["asd", "asd"]) ),
  ?assert( lists:sort(Frequencies) =:= [{$a,2},{$d,2},{$s,2}] ).

many_test() ->
  Frequencies = dict:to_list( parallel_letter_frequency:dict(lists:duplicate(1000, "asd")) ),
  ?assert( lists:sort(Frequencies) =:= [{$a,1000},{$d,1000},{$s,1000}] ).

differ_test() ->
  Frequencies = dict:to_list( parallel_letter_frequency:dict(["abc", "def", "f"]) ),
  ?assert( lists:sort(Frequencies) =:= [{$a,1},{$b,1},{$c,1},{$d,1},{$e,1},{$f,2}] ).

version_test() ->
  ?assertMatch(1, parallel_letter_frequency:test_version()).
