%
%  Successor arithmetic examples
%  If your prof gives you these, complain that toy problems are boring
%

sum(zero,X,X).
sum(s(X), Y, Z):-sum(X,s(Y),Z).

amult(zero, _, zero) :- !.
amult(_, zero, zero) :- !.
amult(s(zero), X, X) :- !.
amult(X, s(zero), X) :- !.
amult(s(X), Y, Z) :-
	amult(X , Y , W),
	sum(Y, W, Z).

factorial(zero,s(zero)).
factorial(s(X),Y):-
	amult(s(X),Y2,Y),
	factorial(X,Y2).
