-module(complex_numbers).

-define(DELTA, 0.005).

-export([abs/1,
         add/2,
         conjugate/1,
         divide/2,
         equal/2,
         exp/1,
         imaginary/1,
         mul/2,
         new/2,
         real/1,
         sub/2,
         test_version/0]).

-record(complex, {real = 0 :: number(), imaginary = 0 :: number()}).
-opaque complex() :: #complex{}.
-export_type([complex/0]).

%% API

-spec abs(complex()) -> number().
abs(#complex{real = R, imaginary = I}) ->
    math:sqrt(math:pow(R, 2) + math:pow(I, 2)).

-spec add(complex(), complex()) -> complex().
add(#complex{real = R1, imaginary = I1}, #complex{real = R2, imaginary = I2}) ->
    #complex{real = R1 + R2, imaginary = I1 + I2}.

-spec conjugate(complex()) -> complex().
conjugate(#complex{real = R, imaginary = I}) ->
    #complex{real = R, imaginary = -I}.

-spec divide(complex(), complex()) -> complex().
divide(#complex{real = R1, imaginary = I1}, #complex{real = R2, imaginary = I2}) ->
    #complex{real = (R1 * R2 + I1 * I2) / (math:pow(R2, 2) + math:pow(I2, 2)),
             imaginary = (I1 * R2 - R1 * I2) / (math:pow(R2, 2) + math:pow(I2, 2))}.

-spec equal(complex(), complex()) -> boolean().
equal(#complex{real = R1, imaginary = I1}, #complex{real = R2, imaginary = I2}) when
        erlang:abs(R2 - R1) =< ?DELTA,
        erlang:abs(I2 - I1) =< ?DELTA ->
    true;
equal(_Z1, _Z2) ->
    false.

-spec exp(complex()) -> complex().
exp(#complex{real = R, imaginary = I}) ->
    #complex{real = math:exp(R) * math:cos(I),
             imaginary = math:exp(R) * math:sin(I)}.

-spec imaginary(complex()) -> number().
imaginary(#complex{imaginary = I}) ->
    I.

-spec mul(complex(), complex()) -> complex().
mul(#complex{real = R1, imaginary = I1}, #complex{real = R2, imaginary = I2}) ->
    #complex{real = R1 * R2 - I1 * I2,
             imaginary = I1 * R2 + R1 * I2}.

-spec new(number(), number()) -> complex().
new(R, I) ->
    #complex{real = R, imaginary = I}.

-spec real(complex()) -> number().
real(#complex{real = R}) ->
    R.

-spec sub(complex(), complex()) -> complex().
sub(#complex{real = R1, imaginary = I1}, #complex{real = R2, imaginary = I2}) ->
    #complex{real = R1 - R2, imaginary = I1 - I2}.

-spec test_version() -> integer().
test_version() ->
    2.
