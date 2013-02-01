%
%%	Example that accepts simple sentences
%

sentence --> subject, w, verb, w, predicate.
sentence --> subject, w, verb.

subject --> noun.
subject --> adjective, w, subject.

adjective --> "tall".
adjective --> "short".
adjective --> "a", w, adjective.

verb --> "runs".
verb --> "lies".

predicate --> "quickly".
predicate --> "quietly".

w --> " ".

noun --> "grass".
noun --> "tree".
noun --> "field".
noun --> "sun".

poem(Poem) :-
	phrase(sentence , Poem).
