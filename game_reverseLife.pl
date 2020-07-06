% https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
% https://ru.wikipedia.org/wiki/Игра_«Жизнь»
% Task : game state is set to a list of live cells(live(x, y)). Need to restore any suitable previous state
% gameR([life(2, 1), life(2, 2)], Ans)
% Ans / [life(1, 1), life(2, 2), life(3, 1)]
% *Order of cells is important :(

rand(X) :- rand_int(2, X).
randxy(X) :- rand_int(3, X).

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
check_cell(X, Y, Z) :- Z is 0.

game(Now, Future) :-
	deleteAll(0),
	asserts_life(Now),
	goList(Now, Future),
	deleteAll(0).

goList([], Future) :- Future = [], !.
goList([life(X, Y) | T], Future) :-
	mode_1(life(X, Y), L1),
	mode_2(life(X, Y), L2, 2, 3),
	goList(T, Future0),
	append(L1, L2, L3),
	append(L3, Future0, Future).

mode_2(life(X, Y), L, CompZ1, CompZ2) :-
	nearby(X, Y, Z),
	((Z = CompZ1, !) ; Z = CompZ2),
	not table_life_future(X, Y),
	assert(table_life_future(X, Y)),
	L = [life(X, Y)], !.

mode_2(life(X, Y), L, _, _) :- L = [].

mode_1(life(X, Y), L) :-
	X1 is X - 1,
	Y1 is Y + 1,
	mode_2(life(X1, Y1), L1, 3, 3),
	X2 is X,
	Y2 is Y + 1,
	mode_2(life(X2, Y2), L2, 3, 3),
	X3 is X + 1,
	Y3 is Y + 1,
	mode_2(life(X3, Y3), L3, 3, 3),
	X4 is X - 1,
	Y4 is Y,
	mode_2(life(X4, Y4), L4, 3, 3),
	X6 is X + 1,
	Y6 is Y,
	mode_2(life(X6, Y6), L6, 3, 3),
	X7 is X - 1,
	Y7 is Y - 1,
	mode_2(life(X7, Y7), L7, 3, 3),
	X8 is X,
	Y8 is Y - 1,
	mode_2(life(X8, Y8), L8, 3, 3),
	X9 is X + 1,
	Y9 is Y - 1,
	mode_2(life(X9, Y9), L9, 3, 3),
	append(L1, L2, LX1),
	append(L3, L4, LX2),
	append(L6, L7, LX3),
	append(L8, L9, LX4),
	append(LX1, LX2, LXX1),
	append(LX3, LX4, LXX2),
	append(LXX1, LXX2, L), !.

mode_1(life(X, Y), L) :- L = [].

deleteAll(_) :-
retractall(table_life(Worse0, Worse1)),
retractall(table_life_future(Worse2, Worse3)).

gameR(Future, Now) :-
	list(Future), list(Now), !,
	game(Now, Future).

gameR(Future, Now) :-
	generateList(Future, Now0),
	game(Now0, Future), 
	Now = Now0, !.

gameR(Future, Now) :- gameR(Future, Now).

generateList([life(X, Y) | T], Now0) :-
	rand(W),
	Cnt is W + 2,
	getCells(X, Y, Cnt, Now0), !.

getCells(_, _, 0, L) :- L = [], !.
getCells(X, Y, Sz, L) :-
	Sz1 is Sz - 1,
	getCells(X, Y, Sz1, L1),
	randxy(WX),
	X1 is X+WX-1,
	randxy(WY),
	Y1 is Y+WY-1,
	append([life(X1, Y1)], L1, L), !.
