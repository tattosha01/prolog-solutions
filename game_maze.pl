% Path in maze

% n(X) : maxI, m(X) : maxJ;
% p(X, Y, Xs, Ys) : (Xs, Ys) -> (X, Y) in answer;
% dp(X, Y, V) : minLengPath to (X, Y);

% findPath(['....', '##..', '....', '..##', '....'], P)
% P / [(1,1),(1,2),(1,3),(2,3),(3,3),(3,2),(4,2),(5,2),(5,3),(5,4)]
% ++++.
% ###+.
% ..++.
% .#+##
% ..+++

findPath([H | T], Path) :-
	retract_all(0),
	assert(dp(1, 1, 0)),
	assert_maze(1, [H | T]),
	dfs(1, 1, 0),
	n(N), m(M),
	recoveryPath(N, M, Path0),
	reverse(Path0, Path),
	outMaze([H | T], Path), !.

assert_maze(I, []) :-
		I1 is I - 1,
		assert(n(I1) :- !), !.

assert_maze(I, [H | T]) :-
	atom_chars(H, ListH),
	assert_cells(I, 1, ListH),
	I1 is I + 1,
	assert_maze(I1, T).

assert_cells(I, J, []) :-
	J1 is J-1,
	assert(m(J1) :- !), !.

assert_cells(I, J, [H | T]) :-
	H = '.',
	assert(f(I, J)),
	J1 is J + 1,
	assert_cells(I, J1, T), !.

assert_cells(I, J, [H | T]) :-
	J1 is J + 1,
	assert_cells(I, J1, T).

retract_all(_) :-
	retractall(f(Wx, Wy)),
	retractall(n(N)),
	retractall(m(M)),
	retractall(p(X, Y, Xs, Ys)),
	retractall(dp(Xx, Yy, V)).

dfs(I, J, V) :-
	n(N), m(M),
	I = N, J = M.

%----------UP----------
dfs(I, J, V) :-
	I1 is I - 1,
	J1 is J,
	V1 is V + 1,
	f(I1, J1),
	not dp(I1, J1, W),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).

dfs(I, J, V) :-
	I1 is I - 1,
	J1 is J,
	V1 is V + 1,
	f(I1, J1),
	dp(I1, J1, W),
	W > V1,
	retract(dp(I1, J1, W)),
	retract(p(I1, J1, WX, WY)),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).
%----------UP----------
%----------RIGHT----------
dfs(I, J, V) :-
	I1 is I,
	J1 is J + 1,
	V1 is V + 1,
	f(I1, J1),
	not dp(I1, J1, W),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).

dfs(I, J, V) :-
	I1 is I,
	J1 is J + 1,
	V1 is V + 1,
	f(I1, J1),
	dp(I1, J1, W),
	W > V1,
	retract(dp(I1, J1, W)),
	retract(p(I1, J1, WX, WY)),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).
%----------RIGHT----------
%----------DOWN----------
dfs(I, J, V) :-
	I1 is I + 1,
	J1 is J,
	V1 is V + 1,
	f(I1, J1),
	not dp(I1, J1, W),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).

dfs(I, J, V) :-
	I1 is I + 1,
	J1 is J,
	V1 is V + 1,
	f(I1, J1),
	dp(I1, J1, W),
	W > V1,
	retract(dp(I1, J1, W)),
	retract(p(I1, J1, WX, WY)),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).
%----------DOWN----------
%----------LEFT----------
dfs(I, J, V) :-
	I1 is I,
	J1 is J - 1,
	V1 is V + 1,
	f(I1, J1),
	not dp(I1, J1, W),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).

dfs(I, J, V) :-
	I1 is I,
	J1 is J - 1,
	V1 is V + 1,
	f(I1, J1),
	dp(I1, J1, W),
	W > V1,
	retract(dp(I1, J1, W)),
	retract(p(I1, J1, WX, WY)),
	assert(dp(I1, J1, V1)),
	assert(p(I1, J1, I, J)),
	findall(dfs(I1, J1, V1), dfs(I1, J1, V1), WORSE).
%----------LEFT----------

recoveryPath(1, 1, Path) :- Path = [(1, 1)], !.
recoveryPath(I, J, Path) :-
	p(I, J, Is, Js),
	recoveryPath(Is, Js, Path0),
	append([(I, J)], Path0, Path).

outMaze([H | T], Path) :- out_by_I(1, [H | T], Path).

out_by_I(_, [], _) :- !.
out_by_I(I, [H | T], Path) :-
	atom_chars(H, ListH),
	out_by_J(I, 1, ListH, Path),
	nl,
	I1 is I + 1,
	out_by_I(I1, T, Path).

out_by_J(_, _, [], _) :- !.
out_by_J(I, J, [H | T], Path) :-
	member((I, J), Path),
	write('+'),
	J1 is J + 1,
	out_by_J(I, J1, T, Path).

out_by_J(I, J, [H | T], Path) :-
	write(H),
	J1 is J + 1,
	out_by_J(I, J1, T, Path).