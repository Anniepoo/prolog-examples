%
% This is in many ways a better solution to the detective puzzle
% See detectivepuzzle.pl for a problem explanation
%
% This was written by Adrian King
%
murderer(X) :- bagof(Y,contradictsPerson(X,Y),Ys), length(Ys,N), N > 1.

contradictsPerson(A,B) :-
claims(A,AClaims), claims(B,BClaims), contradictsClaims(AClaims,BClaims).

contradictsClaims(Claims,[H|T]) :-
contradictsClaims1(Claims,H); contradictsClaims(Claims,T).
contradictsClaims1([H|T],C) :-
contradictsClaim(H,C); contradictsClaims1(T,C).
contradictsClaim([Claim1,Who],[Claim2,Who]) :-
contradictory(Claim1,Claim2).

claims(art,[[innocent,art],[knewVic,burt],[knewVic,carl]]).
claims(burt,[[outOfTown,burt],[didNotKnowVic,burt]]).
claims(carl,[[innocent,carl],[inTown,burt],[inTown,carl]]).

contradictory(innocent,murderer).
contradictory(murderer,innocent).
contradictory(outOfTown,inTown).
contradictory(inTown,outOfTown).
contradictory(knewVic,didNotKnowVic).
contradictory(didNotKnowVic,knewVic).
