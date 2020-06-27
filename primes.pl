% nxt(X, Y) : Y - minimum prime divisor for X
% goSieve(X, N) : builds sieve of Eratosthenes for 2..sqrt(N)
% goSieve2(X, N) : builds sieve of Eratosthenes for sqrt(N)+1..N
% composite(X) : /yes. if X - composite
% prime(X) : /yes. if X - prime
% loop(X, N, S) : helps to build sieve of Eratosthenes. Finds composite numbers. // for (int i = X, i < N; i += S) {}
% sorted(D) : /yes. if List 'D' is sorted
% prime_divisors(X, D) : d_1*d_2*..*d_n === X, d_1<=d_2<=..<=d_n, d_i - prime for all i
% getFactors(N, D) : build list D for 'prime_divisors(X, D)'
% getN(N, D, Ans) : N*d_1*d_2*..*d_n === Ans
% lcm(A, B, LCM) : lcm_1*lcm_2*..*lcm_n === LargestCommonMultiple(A, B), lcm_1<=lcm_2<=..<=lcm_n, lcm_i - prime for all i
% merge(D1, D2, DRes) : merges two lists(D1 and D2) in DRes
% init(N) : The first function for launch. Build sieve of Eratosthenes 1..N

composite(X) :-
	X > 1,
	nxt(X, Y),
	X \= Y.

prime(X) :-
	X > 1,
	\+ composite(X).

goSieve(X, N) :-
	X*X =< N,
	prime(X),
	assert(nxt(X, X)),
	XX is X * X,
	loop(XX, N, X), !.
	
goSieve(X, N) :-
	X*X =< N,
	X1 is X + 1 + mod(X, 2),
	goSieve(X1, N), !.

goSieve2(X, N) :-
  X =< N,
  \+ nxt(X, Y),
  assert(nxt(X, X)),
  false.

goSieve2(X, N) :-
  X =< N,
  X1 is X + 1 + mod(X, 2),
  goSieve2(X1, N).

loop(X, N, S) :-
	X =< N,
	assert(nxt(X, S)),
	X1 is X + S,
	loop(X1, N, S).

sorted([]) :- !.
sorted([H]) :- !.
sorted([H, H1 | T]) :-
  H =< H1,
  sorted([H1 | T]), !.

prime_divisors(1, []) :- !.

prime_divisors(X, D) :-
	integer(X), !, getFactors(X, D).

prime_divisors(X, D) :-
	getN(1, D, X),
	sorted(D),
	getFactors(X, D), !.

getFactors(N, [N]) :- prime(N), !.

getFactors(N, [H | T]) :-
	nxt(N, H), !,
  N1 is div(N, H),
  getFactors(N1, T).

getN(N, [M], Ans) :-
	NM is N * M,
	Ans is NM, !.
	
getN(N, [H | T], Ans) :-
	NH is N * H,
	getN(NH, T, Ans), !.

lcm(1, X, X) :- !.
lcm(X, 1, X) :- !.
lcm(A, B, LCM) :-
	prime_divisors(A, Ds1),
	prime_divisors(B, Ds2),
	merge(Ds1, Ds2, Res),
	getN(1, Res, LCM).

merge([], Res, Res) :- !.
merge([H | T1], [H | T2], [H | Res]) :- !, merge(T1, T2, Res).
merge([H1 | T1], [H2 | T2], Res) :- H2 < H1, !, merge([H2 | T2], [H1 | T1], Res).
merge([H1 | T1], Ds2, [H1 | Res]) :- merge(T1, Ds2, Res).

init(N) :-
	goSieve(2, N);
	(Nsqrt is ceiling(sqrt(N)), goSieve2(Nsqrt, N)).