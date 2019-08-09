:- use_module(library(clpfd)).

nucleotide('A').
nucleotide('C').
nucleotide('G').
nucleotide('T').

count(N, C, (N, C)) :- nucleotide(N), C in 0..sup.

%! nucleotide_count(++Nucleotides:atom, -Counts:[(atom, integer)]) is semidet.
%
%  Count the number of times each nucleotide appears in `Nucleotides` as
%  a list of `Counts`.
nucleotide_count(Nucleotides, Counts) :-
	setof(N, nucleotide(N), Ns),
	maplist(count, Ns, NCounts, Counts),
	sum(NCounts, #=, NLength),
	length(NList, NLength),
	maplist(nucleotide, NList),
	maplist(nucleotide_count(NList, NLength, 0), Ns, NCounts),
	atom_chars(Nucleotides, NList).

nucleotide_count([], 0, Count, _Nucleotide, Count).
nucleotide_count([Nucleotide | Nucleotides], NLength, Count0, Nucleotide, Count) :-
	NLength #> 0,
	NLength1 #= NLength - 1,
	Count1 #= Count0 + 1,
	nucleotide_count(Nucleotides, NLength1, Count1, Nucleotide, Count).
nucleotide_count([N | Nucleotides], NLength, Count0, Nucleotide, Count) :-
	dif(N, Nucleotide),
	NLength #> 0,
	NLength1 #= NLength - 1,
	nucleotide_count(Nucleotides, NLength1, Count0, Nucleotide, Count).
