% M is the murderer
%  a,b, and c are Art, Burt, and Carl
%  W is the current list of witnesses
%

testimony(art, relationship(burt,friend)).
testimony(art, relationship(carl,enemy)).
testimony(burt, town(burt, out)).
testimony(burt, relationship(burt, stranger)).
testimony(carl, town(art,in)).
testimony(carl,town(burt,in)).
testimony(carl,town(carl,in)).

question(town, [in,out]).
question(relationship, [friend, enemy, stranger]).

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



% murderer(?M, +Questions, ?Theory)
%  resolves if M is the murderer under Theory Theory
%  where M is the atom name of the murderer
%  Theory is a list of answers of the form question(person, value)
murderer(Murderer, Theory) :-
	suspect(Innocents , Murderer),
	theory( Theory),
	forall(member(Witness, Innocents),
	       consistent(Witness , Theory)).

suspect([burt,carl], art).
suspect([art,carl], burt).
suspect([art,burt] , carl).
suspects([art,burt,carl]).

