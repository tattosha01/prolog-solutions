% game_set

% https://en.wikipedia.org/wiki/Set_(card_game)
% https://ru.wikipedia.org/wiki/Сет_(игра)

% find_sets([card(red, oval, one, solid), card(green, oval, two, open), card(red, diamond, two, solid), card(purple, squiggle, two, striped), card(green, squiggle, three, open)], Sets)

find_sets(Ds, Sets) :-
	retractall(fset(card(C1, S1, N1, T1), card(C2, S2, N2, T2), card(C3, S3, N3, T3))),
	loop1(Ds),
	findall(fset(card(C1, S1, N1, T1), card(C2, S2, N2, T2), card(C3, S3, N3, T3)), fset(card(C1, S1, N1, T1), card(C2, S2, N2, T2), card(C3, S3, N3, T3)), Sets),
	outAll(1, Sets).

outAll(I, [fset(card(C1, S1, N1, T1), card(C2, S2, N2, T2), card(C3, S3, N3, T3)) | T]) :-
	write('set'), write(I), nl,
	write(C1), write(', '), write(S1), write(', '), write(N1), write(', '), write(T1), nl,
	write(C2), write(', '), write(S2), write(', '), write(N2), write(', '), write(T2), nl,
	write(C3), write(', '), write(S3), write(', '), write(N3), write(', '), write(T3), nl,
	I1 is I + 1,
	outAll(I1, T).

loop1([]) :- !.
loop1([H | T]) :-
	loop2([H], T),
	loop1(T).

loop2(_, []) :- !.
loop2(App, [H | T]) :-
	append(App, [H], App1),
	loop3(App1, T),
	loop2(App, T).

loop3(_, []) :- !.
loop3(App, [H | T]) :-
	append(App, [H], App1),
	isSet(App1),
	card(C1, S1, N1, T1) = H,
	firstInList(App, card(C2, S2, N2, T2)),
	lastInList(App, card(C3, S3, N3, T3)),
	assert(fset(card(C1, S1, N1, T1), card(C2, S2, N2, T2), card(C3, S3, N3, T3))),
	loop3(App, T), !.

loop3(App, [H | T]) :- loop3(App, T).


lastInList([H], H) :- !.
lastInList([H | T], Ans) :-
  lastInList(T, Ans).

firstInList([H | T], H) :- !.

%----------COLOR----------
isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	C1 = C2, C1 = C3, !.

isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	C1 \= C2, C1 \= C3, C2 \= C3, !.
%----------COLOR----------
%----------SYMBOL----------
isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	S1 = S2, S1 = S3, !.

isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	S1 \= S2, S1 \= S3, S2 \= S3, !.
%----------SYMBOL----------
%----------NUMBER----------
isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	N1 = N2, N1 = N3, !.

isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	N1 \= N2, N1 \= N3, N2 \= N3, !.
%----------NUMBER----------
%----------TEXTURE----------
isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	T1 = T2, T1 = T3, !.

isSet([card(C1, S1, N1, T1) | T]) :-
	firstInList(T, card(C2, S2, N2, T2)),
	lastInList(T, card(C3, S3, N3, T3)),
	T1 \= T2, T1 \= T3, T2 \= T3, !.
%----------TEXTURE----------
