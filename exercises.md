# Prolog Exercises

Anne Ogborn

# 1) Complex Math (easy, no cuts or lists)

The 'is' operator isn't special syntax, it's simply an operator defined with op/3.

Define an operator c_is that evaluates complex math (look up complex math ona high school
tutorial site if you don't know it, despite the name it's not complex).

Define a postfix i operator to mean a complex number, so 3+0.5i is the complex number 3+0.5i

Your operator should correctly handle all these.

X c_is 3+2i + 5+i1, X = 8+3i.
X c_is 1+1i + (3+4i)*(3+4i). % X is near 25.70992 + 1.8063i
3+1i c_is 1.5+1i + 1.5 .

# 2) Case Based Reasoning (hard, open ended)

Much of what humans do to think is reason by finding a similar past problem to the one we
face, and reasoning by analogy.

Case based reasoning takes a set of 'cases', representing things that worked in the past,
and attempts to find similar ones in the present.

Since we're unlikely to face exactly the same problem twice, we need to find matches that
are 'sort of close'. 

Let's say we're trying to predict the sentence for Bubba in this case: 

Arnie, quite intoxicated, was drinking at a bar when Bubba came in. They had a dispute over a woman.
Arnie started 'making trouble' with Bubba, pushing him and uttering curses. Bubba tried to defuse the
situation, but instead of leaving, insisted Arnie leave. Arnie hit Bubba with a pool cue. Bubba pulled a knife
and stabbed Arnie in the chest. The ambulance took an usually long time to get there, and a doctor testified
he might have lived if it had gotten there sooner.
Arnie was 19 and had a juvenile record for methamphetamine possession.
Bubba is 22 and has a conviction for illegal possession of a firearm and a DUI.
Bubba is charged with voluntary manslaughter.

Here's another case.

Aaron (age32) and Bob (age 38) were business partners in a failing restaurant. 
Bob arrived one night at the business to find Aaron quite intoxicated. They got into
an argument. Aaron threatened to have Bob 'rubbed out by his mob friends'. 
Bob, expecting trouble, had brought a gun. He shot Aaron in the arm, which he
claimed he did to 'scare off' Aaron. The bullet richocheted off a bone and
went through Aaron's chest, killing him instantly.
Bob was charged with voluntary manslaughter, plead guilty, and received an 8 year prison sentence.
Bob and Aaron were both under investigation as part of an organized crime investigation at
the time of the incident. Bob later plead guilty to an extortion charge in connection with
that investigation and received an additional 2 year sentence.

And we know that the maximum for voluntary manslaughter in California is an 11 year sentence.

(By the way, I am not a lawyer, this is all made up data).

It has some things that are irrelevant (names of parties, it happened at night).
Some things are the same (same charge).
Some things are 'kind of' the same (both victims menaced their killers,
but Arnie actually attacked Bubba, while Bob only threatened to have Aaron killed).
Some things are numeric, but are still 'kind of' the same (Aaron and Bob are adults, they
can't claim their age is a factor - while Arnie and Bubba seem young enough to be 'reformable'.

(Note, this is unfinished, I'll finish later).










