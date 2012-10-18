sentence --> sentence(X).
sentence(X) --> noun_phrase(X), verb_phrase(X).
noun_phrase(X) --> determiner, noun(X).
verb_phrase(X) --> verb(X), noun_phrase(_).
verb_phrase(X) --> verb(X).
determiner --> [the].
verb(singular) --> [eats].
verb(plural) --> [eat].
noun(singular) --> [apple].
noun(plural) --> [apples].
noun(singular) --> [boy].
noun(plural) --> [boys].