:- use_module(library(clpfd)).

addlists([], [], []).

addlists([HA|TA],[HB|TB],[HC|TC]):-
   HC #= HA+HB,
   addlists(TA,TB,TC).


