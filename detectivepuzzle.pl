%
%  This solves the following puzzle
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
%BUGS: this has some bug

%
% Express the testimony of each witness as facts
% The second arg is a complex term
% not a function call
%
testimony(art, relationship(burt,friend)).
testimony(art, relationship(carl,enemy)).
testimony(burt, town(burt, out)).
testimony(burt, relationship(burt, stranger)).
testimony(carl, town(art,in)).
testimony(carl,town(burt,in)).
testimony(carl,town(carl,in)).

% A list of questions and their possible answers
% The subject of the question is one of the
% witnesses (so each witness was in or out of town,
% and each was a friend, enemy, or stranger)
%
question(town, [in,out]).
question(relationship, [friend, enemy, stranger]).

%
% Now we generate a theory, which is a set of answers
% to each question for each witness
theory(Theory) :-
	setof(Q , A^question(Q,A) , AllQuestions),
	theory_generator(AllQuestions , [], Theory).

theory_generator([], Theory, Theory).
theory_generator([Q|T] , TheorySoFar , Theory) :-
	suspects(S),
	question(Q, Answers),
	member(Person, S),
	member(Answer, Answers),
	QQ =.. [Q , Person, Answer],
	theory_generator(T , [QQ|TheorySoFar] , Theory).

%
% Unify if a witness' testimony is consistent with a theory
% of the crime
%
consistent(_Witness, []).
consistent(Witness , [QQ|Theory]) :-
	QQ =.. [Q, P, _],
	suspects(S),
	forall((testimony(Witness , Answer), member(Person, S)),
	       (
	           QQ =.. [Q, Person, Answer],
		   testimony(Witness, QQ)   % testimony agrees
	       ;
	           QQQ =.. [Q, P, _],
		   \+ testimony(Witness, QQQ)   % no testimony on subject
	       )),
	consistent(Witness , Theory).



% murderer(?M, ?Theory)
%  resolves if M is the murderer under Theory Theory
%  where M is the atom name of the murderer
%  Theory is a list of answers of the form question(person, value)
murderer(Murderer, Theory) :-
	suspect(Innocents , Murderer),
	theory( Theory),
	forall(member(Witness, Innocents),
	       consistent(Witness , Theory)).

%
% perhaps poorly named, the possible
% guilty parties
%
%   suspect(-ListOfInnocents, -Guilty)
%
suspect([burt,carl], art).
suspect([art,carl], burt).
suspect([art,burt] , carl).

% all the suspects
suspects([art,burt,carl]).

