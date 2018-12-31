:- module(leq,[leq/2,dom/2]).
:- use_module(library(chr)).

:- chr_constraint leq/2.
reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

/*
 * ?- leq(X,Y),leq(Y,Z),leq(Z,X).
X = Y, Y = Z.

 *
 *
 * ?- leq(X,Y), X = 5,Y = 4.
X = 5,
Y = 4,
leq(5, 4).

Why? Well, you have the attribute leq(5, 4), but no **guard**.
It's the guard that's the actual constraint check.
So it just has added this property that 5 has to be less than 4,
    but no way to actually check.
*/

:- chr_constraint dom(?int,+list(int)).
:- chr_type list(T) ---> [] ; [T|list(T)].

dom(_,[]) <=> fail.
dom(X,[Y]) <=> X = Y.
dom(X,L) <=> nonvar(X) | memberchk(X,L).
dom(X,L1), dom(X,L2) <=> intersection(L1,L2,L3), dom(X,L3).
