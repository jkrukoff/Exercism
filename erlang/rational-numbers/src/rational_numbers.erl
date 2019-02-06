-module(rational_numbers).

-type rational() :: {integer(), integer()}.

-export([absolute/1,
         add/2,
         divide/2,
         exp/2,
         mul/2,
         reduce/1,
         sub/2]).

%% API

-spec absolute(rational()) -> rational().
absolute({N1, D1}) ->
    {abs(N1), abs(D1)}.

-spec add(rational(), rational()) -> rational().
add({N1, D1}, {N2, D2}) ->
    reduce({N1 * D2 + N2 * D1, D1 * D2}).

-spec divide(rational(), rational()) -> rational().
divide({N1, D1}, {N2, D2}) ->
    reduce({N1 * D2, N2 * D1}).

-spec exp(rational(), integer()) -> rational();
         (rational(), float()) -> float();
         (number(), rational()) -> float().
exp({N1, D1}, Exponent) when is_integer(Exponent) ->
    reduce({pow(N1, abs(Exponent)), pow(D1, abs(Exponent))});
exp({N1, D1}, Exponent) when is_float(Exponent) ->
    math:pow(N1, Exponent) / math:pow(D1, Exponent);
exp(Base, {N1, D1}) when is_number(Base) ->
    math:pow(Base, N1 / D1).

-spec mul(rational(), rational()) -> rational().
mul({N1, D1}, {N2, D2}) ->
    reduce({N1 * N2, D1 * D2}).

-spec reduce(rational()) -> rational().
reduce({N, D}) ->
    sign({N div gcd(N, D), D div gcd(N, D)}).

-spec sub(rational(), rational()) -> rational().
sub({N1, D1}, {N2, D2}) ->
    reduce({N1 * D2 - N2 * D1, D1 * D2}).

%% Internal

pow(Base, Exp) when is_integer(Exp), Exp >= 0 ->
    pow(Base, Exp, 1).

pow(_Base, 0, Acc) ->
    Acc;
pow(Base, Exp, Acc) ->
    pow(Base, Exp - 1, Base * Acc).

gcd(0, 0) -> 
    1;
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

sign({N, D}) when D < 0 ->
    {-N, -D};
sign({N, D}) ->
    {N, D}.
