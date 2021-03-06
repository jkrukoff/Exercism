%% based on canonical data version 1.1.0
%% https://raw.githubusercontent.com/exercism/problem-specifications/master/exercises/sieve/canonical-data.json

-module(sieve_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").

no_primes_under_two_test() ->
    ?assertEqual([], sieve:sieve(1)).

first_prime_test() ->
    ?assertEqual([2], sieve:sieve(2)).

primes_up_to_10_test() ->
    ?assertEqual([2, 3, 5, 7], sieve:sieve(10)).

limit_is_prime_test() ->
    ?assertEqual([2, 3, 5, 7, 11, 13], sieve:sieve(13)).

primes_up_to_1000_test() ->
    ?assertEqual(
      [   2,   3,   5,   7,  11,  13,  17,  19,  23,  29,
         31,  37,  41,  43,  47,  53,  59,  61,  67,  71,
         73,  79,  83,  89,  97, 101, 103, 107, 109, 113,
        127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
        179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
        233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
        283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
        353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
        419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
        467, 479, 487, 491, 499, 503, 509, 521, 523, 541,
        547, 557, 563, 569, 571, 577, 587, 593, 599, 601,
        607, 613, 617, 619, 631, 641, 643, 647, 653, 659,
        661, 673, 677, 683, 691, 701, 709, 719, 727, 733,
        739, 743, 751, 757, 761, 769, 773, 787, 797, 809,
        811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
        877, 881, 883, 887, 907, 911, 919, 929, 937, 941,
        947, 953, 967, 971, 977, 983, 991, 997 ],
      sieve:sieve(1000)).

primes_up_to_100000_test() ->
    ?assert(length(sieve:sieve(100000)) > 0).
