% color(i, j, V) : V - value in position gameField[i][j], V = 'W'/'B'/'.'
% black(_) : true, if black
% white(_) : true, if white
% ==> if black(_) and white(_), then both
% ==> if not black(_) and not white(_), then none

% gomoku(['BWWWWW.WWW', 'B.........', 'B.........', 'B.........', 'B.........', '..........', '..........', 'BBB..BBB..', 'WWWW..WWWW', '..........'], Answer)
% BWWWWW.WWW
% B.........
% B.........
% B.........
% B.........
% ..........
% ..........
% BBB..BBB..
% WWWW..WWWW
% ..........

gomoku([H | T], Answer) :-
	retract_all(0),
	assert_feild(1, [H | T]),
	(evalAnsW(1, 1) ; true),
	(evalAnsB(1, 1) ; true),
	checkAnswer(Answer), !.
	
assert_feild(I, []) :- I1 is I - 1, assert(szN(I1) :- !), !.

assert_feild(I, [H | T]) :-
	atom_chars(H, ListH),
	assert_cells(I, 1, ListH),
	I1 is I + 1,
	assert_feild(I1, T).

assert_cells(I, J, []) :- J1 is J - 1, assert(szM(J1) :- !), !.

assert_cells(I, J, [H | T]) :-
	assert(color(I, J, H)),
	J1 is J + 1,
	assert_cells(I, J1, T), !.

retract_all(_) :-
	retractall(color(WI, WJ, WV)),
	retractall(black(Wb)),
	retractall(white(Ww)).

%-------------------------------------------------eval WWW
evalAnsW(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	(isRow(I, J, 'W') ; isColumn(I, J, 'W') ; isGiag(I, J, 'W')),
	assert(white(0)), !.

evalAnsW(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	I1 is I+1,
	evalAnsW(I1, J).

evalAnsW(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	J1 is J+1,
	evalAnsW(I, J1).

evalAnsW(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	I1 is I+1, J1 is J+1,
	evalAnsW(I1, J1).
%-------------------------------------------------eval WWW

%-------------------------------------------------eval BBB
evalAnsB(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	(isRow(I, J, 'B') ; isColumn(I, J, 'B') ; isGiag(I, J, 'B')),
	assert(black(0)), !.

evalAnsB(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	I1 is I+1,
	evalAnsB(I1, J).

evalAnsB(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	J1 is J+1,
	evalAnsB(I, J1).

evalAnsB(I, J) :-
	szN(SzN),
	szM(SzM),
	SzI is SzN - 3,
	SzJ is SzM - 3,
	I < SzI,
	J < SzJ,
	I1 is I+1, J1 is J+1,
	evalAnsB(I1, J1).
%-------------------------------------------------eval BBB

isRow(I, J, X) :-
	color(I, J, X),
	J1 is J+1,
	color(I, J1, X),
	J2 is J+2,
	color(I, J2, X),
	J3 is J+3,
	color(I, J3, X),
	J4 is J+4,
	color(I, J4, X), !.

isColumn(I, J, X) :-
	color(I, J, X),
	I1 is I+1,
	color(I1, J, X),
	I2 is I+2,
	color(I2, J, X),
	I3 is I+3,
	color(I3, J, X),
	I4 is I+4,
	color(I4, J, X), !.

isDiag(I, J, X) :-
	color(I, J, X),
	I1 is I+1, J1 is J+1,
	color(I1, J1, X),
	I2 is I+2, J2 is J+2,
	color(I2, J2, X),
	I3 is I+3, J3 is J+3,
	color(I3, J3, X),
	I4 is I+4, J4 is J+4,
	color(I4, J4, X), !.

checkAnswer(Answer) :-
	black(0),
	white(0),
	Answer = 'both', !.

checkAnswer(Answer) :-
	\+ black(0),
	\+ white(0),
	Answer = 'none', !.

checkAnswer(Answer) :-
	black(0),
	Answer = 'black', !.

checkAnswer(Answer) :-
	white(0),
	Answer = 'white', !.