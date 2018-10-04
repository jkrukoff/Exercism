-module(sieve).

-define(SET_IMPL, gb_sets).

-export([sieve/1]).

%% API

-spec sieve(pos_integer()) -> [pos_integer()].
sieve(1) ->
    [];
sieve(N) when N >= 2 ->
    % Skip the even numbers right from the start.
    sieve(N, lists:seq(3, N, 2), [2]).

%% Internal

sieve(_Last, [], Primes) ->
    lists:reverse(Primes);
sieve(Last, [Prime | Rest], Primes) ->
    Multiples = ?SET_IMPL:from_list(lists:seq(Prime, Last, Prime)),
    sieve(Last,
          [R || R <- Rest, not ?SET_IMPL:is_element(R, Multiples)],
          [Prime | Primes]).
