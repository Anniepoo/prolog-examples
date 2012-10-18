
%   A teacher wishes to seat 16 quarrelsome children
%   in a 4x4 array of chairs
%   Some of the children don't get along
%   The problem is to seat the children so no student
%   is seated adjacent (4 way adjacent) to a child
%   they quarrel with
%
%  The children are numbered 1 thru 16
%  1..8 are girls, 9..16 are boys
%
%
:- use_module(library(clpfd)).


compatible(Neighbor , Student) :-
	Student #= 3   #==> Neighbor #< 9,% #3 doesn't want to sit next to icky boys
	Student #= 5 #==> Neighbor #< 9,% #5 agrees with #3
	Student #= 2 #==> Neighbor #\= 3,% #2 and #3 don't get along
	Student #= 10  #==> Neighbor #> 8.% #10 doesn't like gross girls

constrain_up(1 , _ , _ , _).
constrain_up(R , C , Student , Board) :-
	R > 1,
	NR is R - 1,
	member(seat(NR , C , Neighbor) , Board),
	compatible(Neighbor , Student).

constrain_down(4 , _ , _ , _).
constrain_down(R , C , Student , Board) :-
	R < 4,
	NR is R + 1,
	member(seat(NR , C , Neighbor) , Board),
	compatible(Neighbor , Student).

constrain_left(_ , 1 , _ , _).
constrain_left(R , C , Student , Board) :-
	C > 1,
	NC is C - 1,
	member(seat(R , NC , Neighbor) , Board),
	compatible(Neighbor , Student).

constrain_right(_ , 4 , _ , _).
constrain_right(R , C , Student , Board) :-
	C < 4,
	NC is C + 1,
	member(seat(R , NC , Neighbor) , Board),
	compatible(Neighbor , Student).

constrain_pupil(Board , seat(R , C , Student)) :-
	constrain_up(R , C , Student , Board),
	constrain_down(R , C , Student , Board),
	constrain_left(R , C , Student , Board),
	constrain_right(R , C , Student , Board).

make_seat(R , C , seat(R , C , Student)) :-
	Student in 1..16.

% map between seat(r,c,s) and raw variable
seat_student(seat(_R, _C, S) , S).

% map between the [seat(1,1,S1)...] representaion and the [S1]
% representation
board_students(In , _SoFar , Raw) :-
	maplist(seat_student , In , Raw).

/*
	How to map thru by hand  8cD

board_students([] , _ , []).
board_students([seat(_, _, S)|T] , _ , [S|Vs]) :-
	board_students(T , _ , Vs).
*/

make_board(Board) :-
	findall(S ,
	     (   member(R , [1,2,3,4]) ,
	         member(C , [1,2,3,4]) ,
	         make_seat(R , C , S)) ,
	      Board),
	maplist(seat_student , Board , Raw),
	all_distinct(Raw).

write_board(Board) :-
	member(R , [1,2,3,4]),
	nl,
	member(C , [1,2,3,4]),
	member(seat(R, C, S), Board),
	write(S), write(' '),
	fail.
write_board(_) :- nl.

assign_all_pupils :-
	make_board(Board),
	maplist(constrain_pupil(Board) , Board),
	maplist(seat_student , Board , Raw),
	labeling([], Raw),
	write_board(Board).
