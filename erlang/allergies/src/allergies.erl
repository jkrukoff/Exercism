-module(allergies).

-export([allergies/1,
         is_allergic_to/2,
         test_version/0]).

-type allergen() :: eggs | peanuts | shellfish | strawberries | tomatoes | chocolate | pollen | cats.

%% API

-spec allergies(non_neg_integer()) -> [allergen()].
allergies(Score) ->
    <<Cats:1,
      Pollen:1,
      Chocolate:1,
      Tomatoes:1,
      Strawberries:1,
      Shellfish:1,
      Peanuts:1,
      Eggs:1>> = <<Score:8>>,

    Flags = lists:zip(
              [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats],
              [eggs, peanuts, shellfish, strawberries, tomatoes, chocolate, pollen, cats]),
    [Allergen || {Flag, Allergen} <- Flags, Flag == 1].

-spec is_allergic_to(allergen(), non_neg_integer()) -> boolean().
is_allergic_to(Allergen, Score) ->
    lists:member(Allergen, allergies(Score)).

-spec test_version() -> integer().
test_version() ->
    1.

%% Internal
