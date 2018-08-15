% Every way of arranging N queens in a NxN board so that
% no two queens threaten each other

queens(N, Qs) :-
  numlist(1, N, P),
  findall(Q, (permutation(P, Q), not_diagonal(Q, P)), Qs).

not_diagonal(X, N) :-
  maplist(plus, X, N, Z1),
  maplist(plus, X, Z2, N),
  is_set(Z1),
  is_set(Z2).
