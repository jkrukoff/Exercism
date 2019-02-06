-module(prime_factors).

-export([factors/1]).

%% API

-spec factors(pos_integer()) -> [pos_integer()].
factors(Value) when Value > 0 ->
    factors(2, Value, []).

%% Internal

factors(_Candidate, 1, Acc) ->
    Acc;
factors(Candidate, Value, Acc) when Value rem Candidate == 0 ->
    factors(Candidate, Value div Candidate, [Candidate | Acc]);
factors(2, Value, Acc) ->
    factors(3, Value, Acc);
factors(Candidate, Value, Acc) ->
    % Small optimization to skip even non-primes.
    factors(Candidate + 2, Value, Acc).
