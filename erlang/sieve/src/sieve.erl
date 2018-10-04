-module(sieve).

-export([sieve/1]).

%% API

-spec sieve(pos_integer()) -> [pos_integer()].
sieve(1) ->
    [];
sieve(N) when N >= 2 ->
    sieve(N, lists:seq(2, N), []).

%% Internal

sieve(_Last, [], Primes) ->
    lists:reverse(Primes);
sieve(Last, [Prime | Rest], Primes) ->
    Multiples = multiples(Last, Prime, 1, sets:new()),
    sieve(Last,
          [R || R <- Rest, not sets:is_element(R, Multiples)],
          [Prime | Primes]).

multiples(Last, Prime, Multiple, Multiples) when Prime * Multiple > Last ->
    Multiples;
multiples(Last, Prime, Multiple, Multiples) ->
    multiples(Last,
              Prime,
              Multiple + 1,
              sets:add_element(Prime * Multiple, Multiples)).
