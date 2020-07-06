% https://ru.wikipedia.org/wiki/Города_(игра)

% Task : find the shortest sequence of cities in the list 'Ds'. Start_city : 'S'. Finish_city : 'E'. Answer : 'Ans'

% getAns(['orenburg', 'kirov', 'voronej'], 'krasnoyarsk', 'jeleznogorsk', Ans)
% Ans / [kirov, voronej]

% getAns(['fg', 'def'], 'd', 'g', Ans)
% Ans / no.

% getAns(['fg', 'def'], 'd', 'f', Ans)
% Ans / [def]

lastInList([H], H) :- !.
lastInList([H | T], Ans) :-
  lastInList(T, Ans).

firstInList([H | T], H) :- !.

getAns(Ds, S, S, []) :- !.

getAns(_, S, E, Ans) :-
  atom_chars(S, ListS),
  atom_chars(E, ListE),
  lastInList(ListS, Last),
  firstInList(ListE, First),
  Last = First, !,
  Ans = [].

getAns(Ds, S, E, Res) :-
  atom_chars(S, ListS),
  atom_chars(E, ListE),
  lastInList(ListS, Last),
  firstInList(ListE, First),
  length(Ds, Leng),
  (loop(1, Leng, Ds, Last, First) ; true),
  answer(Res),
  retract(answer(Res)), !.

loop(I, E, Ds, C1, C2) :-
  I =< E,
  \+ check(C1, Ds, C2, I, []),
  I1 is I + 1,
  loop(I1, E, Ds, C1, C2).

check(_, _, _, 0, _) :- !, assert(answer([])).

check(S, [H | T], E, 1, Ans) :-
  atom_chars(H, ListH),
  firstInList(ListH, First),
  lastInList(ListH, Last),
  S = First,
  E = Last, !,
  append(Ans, [H], Ans0),
  assert(answer(Ans0)).

check(S, [H | T], E, Cnt, Ans) :-
  Cnt > 1,
  atom_chars(H, ListH),
  firstInList(ListH, First),
  S = First, !,
  lastInList(ListH, Last),
  Cnt1 is Cnt - 1,
  append(Ans, [H], Ans0),
  check(Last, T, E, Cnt1, Ans0).

check(S, [H | T], E, Cnt, Ans) :-
  Cnt >= 1,
  check(S, T, E, Cnt, Ans), !.
