:- use_module(library(clpfd)).
%
% Simple demo that constrains a list to be the pairwise sum of two other
% lists
%

addlists([], [], []).

addlists([HA|TA],[HB|TB],[HC|TC]):-
   HC #= HA+HB,
   addlists(TA,TB,TC).


