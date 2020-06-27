% treap
% /*C++ code*/ is written for each function

rand(X) :- rand_int(2147483647, X).

node(K, V, Y, L, R, node(K, V, Y, L, R)).

% node* merge(node* t1, node* t2)
merge(nullptr, T, T) :- !.
merge(T, nullptr, T) :- !.

/*
if (t1->y < t2->y)
	t1->r = merge(t1->r, t2);
	return t1;
*/
merge(node(Kl, Vl, Yl, Ll, Rl), node(Kr, Vr, Yr, Lr, Rr), node(Kl, Vl, Yl, Ll, R)) :-
	Yl < Yr,
	merge(Rl, node(Kr, Vr, Yr, Lr, Rr), R), !.

/*
if (t1->y >= t2->y)
	t2->l = merge(t1, t2->l);
	return t2;
*/
merge(node(Kl, Vl, Yl, Ll, Rl), node(Kr, Vr, Yr, Lr, Rr), node(Kr, Vr, Yr, L, Rr)) :-
	merge(node(Kl, Vl, Yl, Ll, Rl), Lr, L).

% pair<node*, node*> split(node* t, int k)
split(nullptr, X, nullptr, nullptr) :- !.

/*
if (t->x <= k)
	var = split(t->r, k);
	t->r = var.first;
	return {t, var.second};
*/
split(node(Kl, Vl, Yl, Ll, Rl), X, node(Kl, Vl, Yl, Ll, First), Second) :-
	X >= Kl,
	split(Rl, X, First, Second), !.

/*
if (t->x > k)
	var = split(t->l, k);
	t->l = var.second;
	return {var.first, t};
*/
split(node(Kl, Vl, Yl, Ll, Rl), X, First, node(Kl, Vl, Yl, Second, Rl)) :-
	split(Ll, X, First, Second).

/*
insert(node*& t, int k)
	var = split(t, k);
	t = merge(merge(var.first, new node(k)), var.second);
*/
map_put(T, K, V, Res) :-
	split(T, K-1, First, Second),
	split(Second, K, Worse, Second2),
	rand(Y),
	NewNode = node(K, V, Y, nullptr, nullptr),
	merge(NewNode, Second2, Merge),
	merge(First, Merge, Res).

/*
delet(node*& t, int k)
	var = split(t, k - 1);
	var2 = split(var.second, k);
	t = merge(var.first, var2.second);
*/
map_remove(T, K, Res) :-
	split(T, K-1, First, Second),
	split(Second, K, Worse, Second2),
	merge(First, Second2, Res).

map_build([], nullptr) :- !.
map_build([(K, V) | T], Res) :-
	map_put(nullptr, K, V, TreeRes),
	map_build(T, ResNext),
	merge(TreeRes, ResNext, Res).

/*
node* find(node* t, int f)
  if (t == nullptr) return nullptr;
  if (t->x == f) return t;
  if (t->x < f) return find(t->r, f);
  return find(t->l, f);
*/
map_get(nullptr, _) :- !, false.
map_get(node(K, V, Y, L, R), K, V) :- !.
map_get(node(K, V, Y, L, R), Kx, Vx) :- K < Kx, map_get(R, Kx, Vx), !.
map_get(node(K, V, Y, L, R), Kx, Vx) :- map_get(L, Kx, Vx).

map_submapSize(nullptr, _, _, 0) :- !.
map_submapSize(node(K, V, Y, L, R), Lb, Rb, Sz) :-
	Lb >= Rb,
	Sz is 0, !.

map_submapSize(node(K, V, Y, L, R), Lb, Rb, Sz) :-
	Lb < Rb,
	getSize(node(K, V, Y, L, R), Lb, Rb, Sz).

getSize(nullptr, _, _, 0) :- !.
getSize(node(K, V, Y, L, R), Lb, Rb, Sz) :-
	K < Lb,
	getSize(R, Lb, Rb, Sz), !.

getSize(node(K, V, Y, L, R), Lb, Rb, Sz) :-
	K >= Rb,
	getSize(L, Lb, Rb, Sz), !.

getSize(node(K, V, Y, L, R), Lb, Rb, Sz) :-
	getSize(L, Lb, K, SzL),
	getSize(R, K, Rb, SzR),
	Sz is SzL + SzR + 1.