%
%  Cannibals and missionaries.
%  starting with some distribution of cannibals and missionaries
%  on the banks of a river, and a canoe that can carry 2 people,
%  move everyone to the right bank of the river
%
% define the 'base' case - when we're done
cannibals(_, 0,0,_,_, _ , List, List).

% cannibals(
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

cannibals(Bank, LC,LM,RC,RM, Visited, List, CompleteList) :-
	canoeCarries(DC, DM),
	% hey, that's a loop, sort of! welcome to backtracking!

	% in prolog variables don't vary, we define new ones for how
	% many of what are on what banks after the move
	% the 'is' does honest to goodness math. Careful, = does NOT do
	% assignment!
	NLM is (LM + Bank * DM),
	NRM is RM - Bank * DM,
	NLC is LC + Bank * DC,
	NRC is RC - Bank * DC,
	% and we make sure there the cannibals don't outnumber the missionaries
	% or that there are no missionaries to eat on the left bank
        (NLC =< NLM ; NLM == 0),
	% same for the right bank
	(NRC =< NRM ; NRM == 0),
	% and we don't want to have negative numbers of anybody anywhere
        NLC >= 0,
	NLM >= 0,
	NRC >= 0,
	NRM >= 0,
	NBank is Bank * -1,
	% and we aren't repeating our old state
	\+ memberchk(canoe(NBank, NLC, NLM) , Visited),
	% and now we have to have a solution from the new position
	cannibals(NBank ,
		  NLC, NLM, NRC, NRM,
		  [canoe(NBank, NLC, NLM) | Visited],
		  [go(NBank, DC, DM) |List], CompleteList).

% now we just enumerate the possible contents of the canoe
%
% canoeCarries( ?CannibalsInCanoe, ?MissionariesInCanoe)
canoeCarries(1,0).
canoeCarries(0,1).
canoeCarries(1,1).
canoeCarries(2,0).
canoeCarries(0,2).

% convenience method for testing
go :-
    cannibals(-1, 3, 3, 0, 0, [canoe(-1,3,3)], [], Complete),
    reverse(Complete , NC),
    showsolution(3 , 3 , 0, 0, NC).

% showsolution(
%        at start we have:
%      +CannibalsOnLeftBank,
%      +MissionariesOnLeftBank,
%      +CannibalsOnRightBank,
%      +MissionariesOnRightBank,
%      +List)  list of moves to print

showsolution(_, _, _, _, []).
showsolution(C, M, RC, RM, [go(-1, DC, DM)|Sol]) :-
	format('~d,~d   \\_______/   ~d,~d~n',
	       [C, M, RC, RM]),
	format('	<-(~d,~d)--~n' , [DC, DM]),
	NLC is C + DC,
	NLM is M + DM,
	NRC is RC - DC,
	NRM is RM - DM,
	showsolution(NLC, NLM, NRC, NRM, Sol).

showsolution(C, M, RC, RM, [go(1, DC, DM)|Sol]) :-
	format('~d,~d   \\_______/   ~d,~d~n',
	       [C,M,RC,RM]),
	format('       --(~d,~d)->~n' , [DC, DM]),
	NLC is C - DC,
	NLM is M - DM,
	NRC is RC + DC,
	NRM is RM + DM,
	showsolution(NLC, NLM, NRC, NRM, Sol).

