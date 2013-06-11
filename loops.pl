%
% A demo of several ways to make a loop
%

%
% recursion
%
recurse([]).
recurse([H|T]) :-
	writeln(H),
	recurse(T).

%
% failure driven loop
%
fail_driven(List) :-
	member(X, List),
	writeln(X),
	fail.
fail_driven(_).

%
%  map
%
by_map(List) :-
	maplist(writeln, List).

%
% Use repeat to make an infinite number of choice points
% here's repeat used to make an interactive top loop
%
interactive_loop:-
  repeat,
  write('Enter command and . (type end. to exit): '),
  read(X),
  write(X), nl,
  % usually we'd do something to parse here
  X = end.

