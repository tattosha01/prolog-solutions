% Colors the vertices of the graph in the minimum number of colors;

% solution([connected(1, [2, 4, 6]), connected(2, [1, 3, 5]), connected(3, [2, 5]), connected(4, [1]), connected(5, [2, 3, 7]), connected(6, [1]), connected(7, [5])], Colors)
% Colors: 1-1, 2-2, 3-1, 4-2, 5-3, 6-2, 7-1
% Edges:
% 1-2
% 1-4
% 1-6
% 2-3
% 2-5
% 3-5
% 5-7

% solution([connected(1, [2, 3, 4]), connected(2, [1, 3, 4, 5]), connected(3, [1, 2, 4]), connected(4, [1, 2, 3]), connected(5, [2])], Colors)
% Colors: 1-1, 2-2, 3-3, 4-4, 5-1
% Edges:
% 1-2
% 1-3
% 1-4
% 2-3
% 2-4
% 2-5
% 3-4

% [H | T] - Adjacency list (Graph), Colors - Answer;
solution([H | T], Colors) :-
  min_colorize([H | T], Colors), !.

% [H | T] - Adjacency list (Graph), Colors - Answer;
min_colorize([H | T], Colors) :- !,
  connected(V, _) = H, 
  assert_graph([H | T]),
  dfs(V),
  findall(color(V1, C1), color(V1, C1), Colors),
  retractall(connected(_, _)),
  retractall(color(_, _)).

% [H | T] - Adjacency list (Graph);
assert_graph([]) :- !.
assert_graph([H | T]) :-
  connected(V, L) = H,
  assert(connected(V, L)),
  assert(color(V, 0)),
  assert_graph(T).

% V - Current vertex;
dfs(V) :- color(V, C), not(C = 0), !.
dfs(V) :- 
  connected(V, L),
  colors_from_vertexes(L, LL),
  allow_color(LL, C),
  retract(color(V, 0)),
  assert(color(V, C)),
  for_dfs(L).

% [H | T] - list of vertex for start DFS
for_dfs([]) :- !.
for_dfs([H | T]) :-
  dfs(H), 
  for_dfs(T).

% L: List of colors in [H | T] (vertexes)
colors_from_vertexes([], []) :- !.
colors_from_vertexes([H | T], L) :-
  color(H, C),
  colors_from_vertexes(T, L1),
  L = [C | L1].

% Color - MEX of List
allow_color(List, Color) :- 
  brute_force(List, Color, 1).

% Tries to substitute the minimum C
brute_force(List, Color, C) :-
  not(member(C, List)), !,
  Color is C.

brute_force(List, Color, C) :-
  C1 is C + 1,
	brute_force(List, Color, C1).
