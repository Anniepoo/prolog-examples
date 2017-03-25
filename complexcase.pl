%
%  This is the same code as newdetective.pl
%  But applied to a more complex case
%  to give a better demo
%
%  I made up some more suspects and more testimony.
%  When I queried murderer, I didn't get any possibilities.
%  So I tried making murderer/2. The small amount of code
%  to extend the problem to consider an accomplice is typical
%  of Prolog.
%
%  You are a detective trying to solve a murder case
%  There are five suspects - Art, Burt, Carl, Dean, Eddie, and Frank
%  They are also the only witnesses
%
%  Here's their statements:
%  Art:
%  Burt was the victim's friend, but the victim and Carl were deadly
%  enemies. Other than that I don't know anything. I was out of town at
%  the time. The victim owed Frank a lot of money, and Frank was sore
%  about that.
%
%  Burt:
%  I was out of town with Eddie when it happened, and on top of that I
%  didn't even know the guy.
%
%  Carl:
%  I'm innocent. I don't know who did it. I saw Art and Burt driving
%  around town then.
%
%  Dean:
%  I don't know anything about it. Yooz all gotta talk to my lawyer.
%
%  Eddie:
%  Art's full of it. The guy owed Frank some trivial amount, Frank
%  thought he was a jerk, but wasn't gonna shoot him over it. I was
%  outta town with Burt
%
%  Frank:
%  I saw Eddie and Burt near the place just before it went down.
%  The guy was a real a__hole, but he owed me like 50 bucks - what,
%  I'm gonna whack him over 50 lousy bucks?
%
%  Determine who is lying.
%
% M is the murderer
%  a,b, and c are Art, Burt, and Carl
%  W is the current list of witnesses
%

%  Art:
%  Burt was the victim's friend, but the victim and Carl were deadly
%  enemies. Other than that I don't know anything. I was out of town at
%  the time. The victim owed Frank a lot of money, and Frank was sore
%  about that.

testimony(a, friend(b)).  % according to Art, Burt was friends with the vic
testimony(a, enemy(c)).
testimony(a, out_of_town(a)).
testimony(a, owed_lots(f)).

%  Burt:
%  I was out of town with Eddie when it happened, and on top of that I
%  didn't even know the guy.

testimony(b, out_of_town(b)).
testimony(b, out_of_town(e)).
testimony(b, stranger(b)).

%  Carl:
%  I'm innocent. I don't know who did it. I saw Art and Burt driving
%  around town then.

testimony(c, in_town(c)). % Carl was in town if he saw Art & Burt
testimony(c, in_town(a)).
testimony(c, in_town(b)).

%
%  Dean:
%  I don't know anything about it. Yooz all gotta talk to my lawyer.
%
%

%  Eddie:
%  Art's full of it. The guy owed Frank some trivial amount, Frank
%  thought he was a jerk, but wasn't gonna shoot him over it. I was
%  outta town with Burt
%

testimony(e, enemy(f)).
testimony(e, trivial_amount(f)).

%  Frank:
%  I saw Eddie and Burt near the place just before it went down.
%  The guy was a real a__hole, but he owed me like 50 bucks - what,
%  I'm gonna whack him over 50 lousy bucks?
testimony(f, in_town(e)).
testimony(f, in_town(b)).
testimony(f, enemy(f)).
testimony(f, trivial_amount(f)).

%
% Now I define what things are inconsistent
%
inconsistent(friend(X), enemy(X)).
inconsistent(friend(X), stranger(X)).
inconsistent(enemy(X), stranger(X)).
inconsistent(out_of_town(X), in_town(X)).
inconsistent(trivial_amount(X), owed_lots(X)).

%%	murderer(?Suspect:atom) is nondet
%
% main call
%
% Suspect is the suspect
%
% @arg Suspect the mug what done the crime
%

murderer(M) :-
	member(M, [a, b, c, d, e, f]), % pick somebody
	select(M, [a, b, c, d, e, f], Witnesses), % ignore their testimony by
					  % removing them from witness list
	consistent(Witnesses).   % if their story is consistent, M is a suspect


%%	murderer(?Suspect:atom, ?Accomplice:atom) is nondet
%
% Try it with a pair of accomplices
%
% Suspect is the suspect
%
% @arg Suspect the mug what done the crime
%

murderer(M, A) :-
	Everybody = [a, b, c, d, e, f],
	member(M, Everybody), % pick somebody
	select(M, Everybody , Others),
	member(A, Others),
	select(A, Others, Witnesses),
	consistent(Witnesses).   % if their story is consistent, M is a suspect


%
% A set of witnesses are consistent if among them there is no
% inconsistent testimony
%
consistent(W) :-
	\+ inconsistent_testimony(W).

%
% A set of witnesses have inconsistent testimony
% if two pieces of testimony belonging to two different
% witnesses are inconsistent
%
inconsistent_testimony(W) :-
	member(X, W),  % X and Y are witnesses
	member(Y, W),
	X \= Y,	       % and are different
	testimony(X, XT), % a piece of X's testimony
	testimony(Y, YT), % a piece of Y's testimony
	inconsistent(XT, YT).  % that are inconsistent
