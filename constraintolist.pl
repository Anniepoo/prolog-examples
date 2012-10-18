:- use_module(library(clpfd)).

%
%  We have the problem of wanting to create constraints
%  dynamicly at runtime.  My example problem is to constrain
%  a variable to one of these values
one_of([1,4,5,6,11,16,21,49,87]).
%  assuming the list is long, or entered by the user, or otherwise
%  we can't use
%  (A #= 1) #\/ (A #= 4) #\/ .....
%  type syntax

% The solution is to create a term of form
%   \/(1, \/(4, \/(5, .... )))
%
% And pass this to in
% in takes ranges and or's, e.g.
%   Foo in 1..7 \/ 19..33
%
%  My example constrains 4 variables to be one of these
%  values, to all be different, and demands that one of
%  the variables be 49
%
% good parts of this answer courtesy Jan Burse
% defects by Anne Ogborn

% list_to_set(+List,-SWI_Set)
list_to_set([X],X).
list_to_set([X,Y|Z],X\/T) :- list_to_set([Y|Z],T).

% list_to_constrain(+Variable,+List)
list_to_constrain(V,X) :- list_to_set(X,S), V in S.

constrain_one_must_be_n(List, N) :-
	or_constraint_list(List, N, CList),
	call(CList).

or_constraint_list([X], N, X #= N).
or_constraint_list([H|T], N, (H #= N) #\/ OT) :-
	or_constraint_list(T, N, OT).

constrain_list_to_list([], _).
constrain_list_to_list([H|T], Values) :-
	list_to_constrain(H, Values),
	constrain_list_to_list(T, Values).

% example, select 4 different values from among one_of
example :-
	Vars = [_,_,_,_],
	one_of(Values),
	all_different(Vars),
	constrain_list_to_list(Vars, Values),
	constrain_one_must_be_n(Vars, 49),
	labeling([], Vars),
	format('~w~n', [Vars]).
