%
%  Cannibals and missionaries.
%  starting with some distribution of cannibals and missionaries
%  on the banks of a river, and a canoe that can carry 2 people,
%  move everyone to the right bank of the river
%
%  the missionaries don't really trust the cannibals, and so want to
%  plan it so there's never more cannibals than missionaries on one side
%  of the river
%
%  Notational convention - in many places I've used
%  variables like NLM, LC, etc. L and R are the left and right banks,
%  respectively, while M and C are, predictably, missionaries and
%  cannibals. N is the semi-standard prefix denoting the new value of a
%  variable.

% abstract the starting conditions
% start(-LC, -LM, -RC, -RM).

start(config(3, 3, 0, 0)).

% convenience method for testing
go :- start(Config),
    moves_to_cross(Config, Moves),
    show_solution(Config, Moves).

count_number_of_ways_to_cross(N) :-
	start(Config),
	setof(Moves, moves_to_cross(Config, Moves), Moveset),
	length(Moveset, N).

show_unique_solutions :-
	start(Config),
	setof(Moves, moves_to_cross(Config, Moves), UniqueMoves),
	member(AMove, UniqueMoves),
	format('======== Solution ===========~n'),
	show_solution(Config, AMove),
	fail.

show_shortest_solution :-
	start(Config),
	setof(Moves, moves_to_cross(Config, Moves), UniqueMoves),
	shortest(9999, UniqueMoves , [], Short),
	show_solution(Config, Short).

shortest(_, [], In, In).

shortest(InLen, [H|T], _, Out) :-
	length(H, L),
	L < InLen,
	shortest(L, T, H, Out).

shortest(InLen, [H|T], In, Out) :-
	length(H, L),
	L >= InLen,
	shortest(InLen, T , In, Out).


% canoe_carries_c_m( ?CannibalsInCanoe, ?MissionariesInCanoe)
% can the canoe carry C cannibals and M missionaries?
canoe_carries_c_m(C,M) :-
	canoe_carries_c_m(C,M,2).

% canoe_carries_c_m( ?CannibalsInCanoe, ?MissionariesInCanoe)
 /*
canoe_carries_c_m(1,0).
canoe_carries_c_m(0,1).
canoe_carries_c_m(1,1).
canoe_carries_c_m(2,0).
canoe_carries_c_m(0,2).
*/

% canoe_carries_c_m ( ?CannibalsInCanoe, ?MissionariesInCanoe,
% +CanoeCapacity)

canoe_carries_c_m(C,M,Capacity) :-
	between(0,Capacity,C),
	between(0,Capacity,M),
	(M = 0 ; C =< M),
	Total is C + M,
	between(1,Capacity, Total).

moves_to_cross(config(LC, LM, RC, RM), Moves) :-
    moves_to_cross(-1, LC, LM, RC, RM, [canoe(-1, LC, LM)], [], Complete),
    reverse(Complete , Moves).

% moves_to_cross(
%           +Bank,   -1 canoe is on left bank 1 canoe is on right bank
%           +CannibalsOnLeftBank,
%           +MissionariesOnLeftBank,
%           +CannibalsOnRightBank,
%           +MissionariesOnRightBank,
%           +Visited  - a list of already visited states.
%               Each state is of the form canoe(B,C,M),
%                  where B is the -1 (left bank) or 1 (right bank)
%                     C is cannibals on the left bank,
%                     M is missionaries on the left bank.
%	    +List - the reverse order list of complete moves
%	          of the form go(B, C, M)
%	          where B is 1 going right and -1 going left
%	          and C and M are the number of cannibals and
%	               missionaries in the canoe
%	    -CompleteList) - the complete solution in the same format
%		  as List

moves_to_cross(_, 0, 0, _, _, _, List, List).

moves_to_cross(Bank, LC,LM,RC,RM, Visited, List, CompleteList) :-
	canoe_carries_c_m(DC, DM),

	NLM is LM + Bank * DM,
	NRM is RM - Bank * DM,
	NLC is LC + Bank * DC,
	NRC is RC - Bank * DC,

	(NLC =< NLM ; NLM == 0),
	(NRC =< NRM ; NRM == 0),

        NLC >= 0,
	NLM >= 0,
	NRC >= 0,
	NRM >= 0,

	NBank is Bank * -1,
	\+ memberchk(canoe(NBank, NLC, NLM) , Visited),

	moves_to_cross(NBank ,
		  NLC, NLM, NRC, NRM,
		  [canoe(NBank, NLC, NLM) | Visited],
		  [go(NBank, DC, DM) |List], CompleteList).

% show_solution(
%        at start we have:
%      +CannibalsOnLeftBank,
%      +MissionariesOnLeftBank,
%      +CannibalsOnRightBank,
%      +MissionariesOnRightBank,
%      +List)  list of moves to print

show_solution(_, []).
show_solution(config(C, M, RC, RM), [go(-1, DC, DM)|Sol]) :-
	format('~d,~d   \\_______/   ~d,~d~n',
	       [C, M, RC, RM]),
	format('	<-(~d,~d)--~n' , [DC, DM]),
	NLC is C + DC,
	NLM is M + DM,
	NRC is RC - DC,
	NRM is RM - DM,
	show_solution(config(NLC, NLM, NRC, NRM), Sol).

show_solution(config(C, M, RC, RM), [go(1, DC, DM)|Sol]) :-
	format('~d,~d   \\_______/   ~d,~d~n',
	       [C,M,RC,RM]),
	format('       --(~d,~d)->~n' , [DC, DM]),
	NLC is C - DC,
	NLM is M - DM,
	NRC is RC + DC,
	NRM is RM + DM,
	show_solution(config(NLC, NLM, NRC, NRM), Sol).



