-module(sieve).

-define(SET_IMPL, gb_sets).

-export([sieve/1]).

%% API

-spec sieve(pos_integer()) -> [pos_integer()].
sieve(1) ->
    [];
sieve(N) when N >= 2 ->
    {_, Primes} = lists:foldl(
        fun (Candidate, {Rejected, Primes}) ->
            case ?SET_IMPL:is_element(Candidate, Rejected) of
                true ->
                    {Rejected, Primes};
                false ->
                    % As an optimization, we're only required to mark
                    % values from the square of the candidate in
                    % consideration, instead of starting from the
                    % first multiple of the candidate value.
                    StartMarking = Candidate * Candidate,
                    NextRejected = case StartMarking > N of
                        true ->
                            Rejected;
                        false ->
                            Multiples = ?SET_IMPL:from_list(
                                lists:seq(StartMarking, N, Candidate)),
                            ?SET_IMPL:union(Rejected, Multiples)
                    end,
                    {NextRejected, [Candidate | Primes]}
            end
        end,
        % Skip the even numbers right from the start.
        {?SET_IMPL:new(), [2]},
        lists:seq(3, N, 2)),
    lists:reverse(Primes).
