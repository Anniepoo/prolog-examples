%
% This is in many ways a better solution to the detective puzzle

%
%  You are a detective trying to solve a murder case
%  There are three suspects - Art, Burt, and Carl
%  They are also the only three witnesses
%
%  Here's their statements:
%  Art:
%  Burt was the victim's friend, but the victim and carl were deadly
%  enemies.
%
%  Burt:
%  I was out of town when it happened, and on top of that I didn't even
%  know the guy.
%
%  Carl:
%  I'm innocent. I don't know who did it. I saw Art and Burt driving
%  around town then.
%
%  Determine who is lying.
%
% M is the murderer
%  a,b, and c are Art, Burt, and Carl
%  W is the current list of witnesses
%

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
