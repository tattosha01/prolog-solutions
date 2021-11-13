% getMaxCycle(Cycle)
% Cycle / [3,1,2,1,3]
% Output: [3,1,2,1,3]

edge(1, 2).
edge(2, 1).
edge(1, 3).
edge(3, 1).
edge(2, 3).

getMaxCycle(Cycle) :-
	assert(cycle([])),
  getCountRules(Rules),
  getMaxNode(Nodes),
  for(1, Nodes, Rules),
  maxCycle(Cycle),
  retractall(cycle(_)),
  write(Cycle), nl, !.

getCountRules(N) :- !,
	findall(edge(A, B), edge(A, B), L),
	length(L, N).

getMaxNode(N) :- !,
	findall(edge(A, B), edge(A, B), L),
	maxInEdgeList(L, N).

max(A, B, A) :- A > B, !.
max(A, B, B).

maxInEdgeList([], 0).
maxInEdgeList([H | T], RealMax) :-
	H = edge(U, V),
	max(U, V, MX1),
	maxInEdgeList(T, MX2),
	max(MX1, MX2, RealMax).

maxList([], [], []).
maxList(A, [], A).
maxList([], A, A).
maxList(A, B, A) :-
	length(A, LA),
	length(B, LB),
	LA > LB.
maxList(A, B, B).

maxFromList([], []).
maxFromList([H | T], C) :-
	H = cycle(L1),
	maxFromList(T, L2),
	maxList(L1, L2, C).	

maxCycle(C) :-
  findall(cycle(X), cycle(X), L),
  maxFromList(L, C).

for(I, N, R) :-
	I = N, !,
	R1 is R + 1,
	findCycle([I], N, R1).

for(I, N, R) :-
	R1 is R + 1,
	findCycle([I], N, R1),
	I1 is I + 1,
	for(I1, N, R).

findCycle(L, N, E) :-
	length(L, Leng),
	Leng = E, !,
	tryAddCycle(L).

findCycle(L, N, E) :-
	tryAddCycle(L),
	forCycle(1, L, N, E).

forCycle(I, L, N, E) :-
	I = N, !,
	append(L, [I], LNew),
	findCycle(LNew, N, E).

forCycle(I, L, N, E) :-
	append(L, [I], LNew),
	findCycle(LNew, N, E),
	I1 is I + 1,
	forCycle(I1, L, N, E).

notRepeat([]).
notRepeat([H | T]) :- length(T, S), S < 3.
notRepeat([H | T]) :-
	T = [H1 | _],
	not edgeAhead(T, H, H1),
	notRepeat(T).

edgeAhead(L, A, B) :-
	length(L, S),
	S > 1,
	L = [H | T],
	T = [H1 | _],
	(edgeAhead(T, A, B) ; H = A, H1 = B).

isCycle([], _).
isCycle([H | T], F) :-
	T = [H1 | T1],
	length(T1, 0), !,
	edge(H, H1),
	H1 = F.
isCycle([H | T], F) :-
	T = [H1 | T1],
	edge(H, H1),
	isCycle(T, F).

tryAddCycle(L) :-
  notRepeat(L),
  L = [H | _],
	isCycle(L, H),
	assert(cycle(L)).
tryAddCycle(_).
