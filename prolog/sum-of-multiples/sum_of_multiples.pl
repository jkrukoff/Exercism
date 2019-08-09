:- use_module(library(clpfd)).

%! sum_of_multiples(++Factors:[integer], ++Limit:integer, -Sum) is det.
% Calculate the `Sum` of all integers between [0, `Limit`) that are multiples
% of one of the factors in `Factors`.
sum_of_multiples(Factors, Limit, Sum) :-
	% I've been able to figure out how to make multiple a pure relation,
	% but I've been unable to figure out a solution for the sum that is
	% also pure. Allowing for multiple duplicate factors appears to
	% complicate things.
	setof(Multiple, (multiple(Factors, Limit, Multiple), indomain(Multiple)), Multiples),
	sum(Multiples, #=, Sum).

%! multiple(-Factors:[integer], -Limit:integer, -Multiple) is multi.
% Test if `Multiple` is a value between [0, `Limit`) and a multiple of one of
% the factors in `Factors`.
multiple([], _Limit, 0).
multiple([Factor | Factors], Limit, Multiple) :-
	Multiple #>= 0,
	Multiple #< Limit,
	(Multiple rem Factor #= 0; multiple(Factors, Limit, Multiple)).
