% Square:
% game([life(1, 1), life(1, 2), life(2, 2)], 1, Ans)
% Ans / [life(1, 1), life(1, 2), life(2, 1), life(2, 2)]
% game([life(1, 1), life(1, 2), life(2, 2)], 2, Ans)
% Ans / [life(1, 1), life(1, 2), life(2, 1), life(2, 2)]

% Cycle:
% game([life(2, 3), life(3, 3), life(4, 3)], 1, Ans)
% Ans / [life(3, 4), life(3, 3), life(3, 2)]
% game([life(2, 3), life(3, 3), life(4, 3)], 2, Ans)
% Ans / [life(2, 3), life(3, 3), life(4, 3)]
% game([life(2, 3), life(3, 3), life(4, 3)], 3, Ans)
% Ans / [life(3, 4), life(3, 3), life(3, 2)]

% Glider:
% game([life(1, 7), life(2, 6), life(3, 6), life(3, 7), life(3, 8)], 1, Ans)
% Ans / [life(2,8),life(2,6),life(3,7),life(3,6),life(4,7)]
% game([life(1, 7), life(2, 6), life(3, 6), life(3, 7), life(3, 8)], 2, Ans)
% Ans / [life(3,8),life(3,6),life(2,6),life(4,6),life(4,7)]
% game([life(1, 7), life(2, 6), life(3, 6), life(3, 7), life(3, 8)], 3, Ans)
% Ans / [life(2,7),life(4,7),life(3,5),life(3,6),life(4,6)]
% game([life(1, 7), life(2, 6), life(3, 6), life(3, 7), life(3, 8)], 4, Ans)
% Ans / [life(2,6),life(4,6),life(4,7),life(4,5),life(3,5)]

game(Now, P, Future) :- game_(Now, Future, 1, P), !.

game_(Now, Future, I, P) :-
	I is P, !,
	deleteAll(0),
	asserts_life(Now),
	goList(Now, Future),
	deleteAll(0).

game_(Now, Future, I, P) :-
	deleteAll(0),
	asserts_life(Now),
	goList(Now, Future1),
	I1 is I + 1,
	game_(Future1, Future, I1, P),
	deleteAll(0).

asserts_life([]) :- !.

asserts_life([life(X, Y) | T]) :-
	not table_life(X, Y),
	assert(table_life(X, Y)),
	asserts_life(T), !.
	
asserts_life([life(X, Y) | T]) :-
	asserts_life(T).

nearby(X, Y, Cnt) :-
	X1 is X - 1,
	Y1 is Y + 1,
	check_cell(X1, Y1, Z1),
	X2 is X,
	Y2 is Y + 1,
	check_cell(X2, Y2, Z2),
	X3 is X + 1,
	Y3 is Y + 1,
	check_cell(X3, Y3, Z3),
	X4 is X - 1,
	Y4 is Y,
	check_cell(X4, Y4, Z4),
	X6 is X + 1,
	Y6 is Y,
	check_cell(X6, Y6, Z6),
	X7 is X - 1,
	Y7 is Y - 1,
	check_cell(X7, Y7, Z7),
	X8 is X,
	Y8 is Y - 1,
	check_cell(X8, Y8, Z8),
	X9 is X + 1,
	Y9 is Y - 1,
	check_cell(X9, Y9, Z9),
	Cnt is Z1+Z2+Z3+Z4+Z6+Z7+Z8+Z9.

check_cell(X, Y, Z) :- table_life(X, Y), Z is 1, !.
check_cell(X, Y, 0).

goList([], []).
goList([life(X, Y) | T], Future) :-
	mode_1(X, Y, L1),
	goList(T, Future0),
	append(L1, Future0, Future).

mode_2(X, Y, L, CompZ1, CompZ2) :-
	nearby(X, Y, Z),
	(Z is CompZ1 ; Z is CompZ2),
	not table_life_future(X, Y),
	assert(table_life_future(X, Y)),
	L = [life(X, Y)], !.

mode_2(X, Y, [], _, _).

mode_1(X, Y, L) :-
	% ---continue live---
	mode_2(X, Y, L0, 2, 3),

	% ---new life---
	X1 is X - 1,
	Y1 is Y + 1,
	mode_2(X1, Y1, L1, 3, 3),
	X2 is X,
	Y2 is Y + 1,
	mode_2(X2, Y2, L2, 3, 3),
	X3 is X + 1,
	Y3 is Y + 1,
	mode_2(X3, Y3, L3, 3, 3),
	X4 is X - 1,
	Y4 is Y,
	mode_2(X4, Y4, L4, 3, 3),
	X6 is X + 1,
	Y6 is Y,
	mode_2(X6, Y6, L6, 3, 3),
	X7 is X - 1,
	Y7 is Y - 1,
	mode_2(X7, Y7, L7, 3, 3),
	X8 is X,
	Y8 is Y - 1,
	mode_2(X8, Y8, L8, 3, 3),
	X9 is X + 1,
	Y9 is Y - 1,
	mode_2(X9, Y9, L9, 3, 3),
	append(L1, L2, LX1),
	append(L3, L4, LX2),
	append(L6, L7, LX3),
	append(L8, L9, LX4),
	append(LX1, LX2, LXX1),
	append(LX3, LX4, LXX2),
	append(LXX1, LXX2, LXX3), 
	append(LXX3, L0, L), !.

mode_1(X, Y, []).

deleteAll(_) :-
	retractall(table_life(Worse0, Worse1)),
	retractall(table_life_future(Worse2, Worse3)).