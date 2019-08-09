coordinate(C) :- between(0, 7, C).

%! create((-X:integer, -Y:integer)) is multi.
% Create a valid location for a queen on the chessboard.
create((X, Y)) :- coordinate(X), coordinate(Y).

%! attack((-Q1:(integer, integer), -Q2:(integer, integer))) is multi.
% Find pairs of chessboard locations where two queens threaten each other with
% a valid attack.
attack(Q1, Q2) :-
	create(Q1),
	create(Q2),
	Q1 \== Q2,
	once(horizontal(Q1, Q2); vertical(Q1, Q2); diagonal(Q1, Q2)).

horizontal((X, _), (X, _)).

vertical((_, Y), (_, Y)).

diagonal((X1, Y1), (X2, Y2)) :- 
	DeltaY is Y2 - Y1,
	DeltaX is X2 - X1,
	DeltaX =\= 0,
	divmod(DeltaY, DeltaX, Div, Rem),
	% If our slope is exactly 1 or -1, the two points are on a diagonal
	% line.
	1 is abs(Div),
	0 = Rem.
