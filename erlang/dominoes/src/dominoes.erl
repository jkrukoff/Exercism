-module(dominoes).

-export([can_chain/1]).

-type domino() :: {pos_integer(), pos_integer()}.

%% API

-spec can_chain([domino()]) -> boolean().
can_chain([]) ->
    true;
can_chain([{D1, D2} | Dominoes]) ->
    can_chain(D1, D2, Dominoes) orelse can_chain(D2, D1, Dominoes).

%% Internal

can_chain(_First, unmatched, []) ->
    false;
can_chain(First, First, []) ->
    true;
can_chain(_First, _Last, []) ->
    false;
can_chain(First, Last, Dominoes) ->
    lists:any(
        fun (D) -> can_chain(First, match(D, Last), Dominoes -- [D]) end,
        Dominoes).

match({D1, D2}, D1) ->
    D2;
match({D1, D2}, D2) ->
    D1;
match(_, _) ->
    unmatched.
