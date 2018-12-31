:- module(leq,[leq/2,dom/2, num/1, inttype/1]).
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


:- chr_constraint num/1, inttype/1.

/*
 *  whoot - it works
 *
 *  What this says
 * If the constraint num is applied to
 * X, then at application time, and
 * when X is bound or has additional
 * constraints applied, try the guard
 * goal (nonvar(X) - we bound X) and
 * if it succeeds, remove the num
 * constraint and call the body as prolog.
 * if the body fails, the thing doing the constraining fails.
 */

num(X) <=> nonvar(X) | number(X).

% this works just like above, but constrains to an integer
inttype(X) <=> nonvar(X) | integer(X).


% discard the num if both num and inttype are applied
%
% if it's an integer and a number, it's just an integer
% we don't need the num constraint
% the inttype is on the left its to be kept,
% num on the right is discarded
% the body is a dummy, we've done what we needed.
% and want this to always succeed
inttype(X) \ num(X) <=>  true.
