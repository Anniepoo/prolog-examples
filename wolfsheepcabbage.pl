:- module(wsc, [go/1]).

go(Moves) :-
   allowed(A),
   list_to_ord_set(A, OA),
   wsc(0,0,0,0, OA, Moves).

allowed([
    s(0,0,0,0),
    s(0,0,0,1),
   % s(0,0,1,1),
    s(0,0,1,0),
  %  s(0,1,1,0),
 %   s(0,1,1,1),
    s(0,1,0,1),
    s(0,1,0,0),
 %   s(1,1,0,0),
    s(1,1,0,1),
    s(1,1,1,1),
    s(1,1,1,0),
    s(1,0,1,0),
    s(1,0,1,1)
%    s(1,0,0,1),
%    s(1,0,0,0)
]).

wsc(1,1,1,1, _, []).   % goal
wsc(0,W,S,C,A, [lr(f) | Rest]) :-  % farmer moving alone
   c(0,W,S,C,A,NA),
    wsc(1,W,S,C, NA, Rest).
wsc(1,W,S,C,A, [rl(f) | Rest]) :-
   c(1,W,S,C,A,NA),
    wsc(0,W,S,C, NA, Rest).
wsc(0,0,S,C,A, [lr(w) | Rest]) :-  % move wolf
    c(0,0,S,C,A,NA),
    wsc(1,1,S,C, NA, Rest).
wsc(1,1,S,C,A, [rl(w) | Rest]) :-
   c(1,1,S,C,A,NA),
    wsc(0,0,S,C, NA, Rest).
wsc(0,W,0,C,A, [lr(s) | Rest]) :-  % move sheep
   c(0,W,0,C,A,NA),
    wsc(1,W,1,C, NA, Rest).
wsc(1,W,1,C,A, [rl(s) | Rest]) :-
   c(1,W,1,C,A,NA),
    wsc(0,W,0,C, NA, Rest).
wsc(0,W,S,0,A, [lr(c) | Rest]) :-  % move cabbage
   c(0,W,S,0,A,NA),
    wsc(1,W,S,1, NA, Rest).
wsc(1,W,S,1,A, [rl(c) | Rest]) :-
   c(1,W,S,1,A,NA),
    wsc(0,W,S,0, NA, Rest).

c(F,W,S,C,A,NA) :-
   safe(F,W,S),
   safe(F,S,C),
   member(s(F,W,S,C), A),
   ord_subtract(A, [s(F,W,S,C)], NA).

safe(A,A, _).
safe(_,0,1).
safe(_,1,0).







