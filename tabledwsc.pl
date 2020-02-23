go(Moves) :-
   wsc(0,0,0,0, Moves).

:- table wsc(_, _, _, _, lattice(shortest/3)).

shortest(P1, P2, P):-
    length(P1, L1),
    length(P2, L2),
    ( L1 < L2 -> P = P1 ; P = P2 ).

% wsc/5, c/4, safe/3 are the same as OP's code, slightly reformatted.

wsc(1,1,1,1, []).               % goal
wsc(0,W,S,C, [lr(f) | Rest]) :- % farmer moving alone
    c(0,W,S,C),
    wsc(1,W,S,C, Rest).
wsc(1,W,S,C, [rl(f) | Rest]) :-
    c(1,W,S,C),
    wsc(0,W,S,C, Rest).
wsc(0,0,S,C, [lr(w) | Rest]) :- % move wolf
    c(0,0,S,C),
    wsc(1,1,S,C, Rest).
wsc(1,1,S,C, [rl(w) | Rest]) :-
    c(1,1,S,C),
    wsc(0,0,S,C, Rest).
wsc(0,W,0,C, [lr(s) | Rest]) :- % move sheep
    c(0,W,0,C),
    wsc(1,W,1,C, Rest).
wsc(1,W,1,C, [rl(s) | Rest]) :-
    c(1,W,1,C),
    wsc(0,W,0,C, Rest).
wsc(0,W,S,0, [lr(c) | Rest]) :- % move cabbage
    c(0,W,S,0),
    wsc(1,W,S,1, Rest).
wsc(1,W,S,1, [rl(c) | Rest]) :-
    c(1,W,S,1),
    wsc(0,W,S,0, Rest).

c(F,W,S,C) :-
    safe(F,W,S),
    safe(F,S,C).

safe(A,A, _).
safe(_,0,1).
safe(_,1,0).
