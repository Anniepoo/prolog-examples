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

# 2) Case Based Reasoning (very hard, open ended)

Much of what humans do to think is reason by finding a similar past problem to the one we
face, and reasoning by analogy.

Case based reasoning takes a set of 'cases', representing things that worked in the past,
and attempts to find similar ones in the present.

Since we're unlikely to face exactly the same problem twice, we need to find matches that
are 'sort of close'. 

As a baby version of this, let's take a bunch of sentences, and find ones that are 'like' each other.

assume we have sentences - case normalized, punctuation discarded.

    [ i, went, to, the, ball, game ].
    [ bob, had, sore, on, his, foot ].
    [ carol, and, alice, went, to, welding, class ].
    [ a, book, fell, from, the, shelf ].
    [ alice, fell, from, the, roof ].
    [ alice, has, a, cat ].
    [ george, has, a, white, dog ].
    [ george, went, to, a, sports, game ].
    [ carol, went, over, to, the, cat ].

and so on. This list is short, feel free to add your own set of test sentences. I strongly suggest using 
a small vocabulary to keep the problem tractable. Your solution must work with new data, of course,
not just with this fixed set, but you can assume the vocabulary won't increase (so "alice has a roof" is fair,
but "ships enter the shipyard constantly" is not).

If you want to be able to enter sentences in normal form, the pack (packs are SWI-Prolog's equivilent of gems) [http://www.swi-prolog.org/pack/list?p=tokenize](tokenize) would be an easy way of getting tha above form from your string.

Find matches that are 'sort of close'. Note there's no right answer to this. "A book fell from the shelf."
and "Alice fell from the roof.", are those closer than "Alice has cat" and "Carol went to the cat."?

Here are some ways to approach the problem:

Make a hierarchy of things. Bob and Carol are people, people are things.
Make substitutions for more general things. So [a_person, fell, from, a_place]
should each thing be in a single class, like Java class inheritence, or in
many classes, like properties - cadillacs are cars, but also possessions, steel things,
drivable things, etc?

Make as many substitutions as you need to in order to make the sentences match.

A problem with this approach is that everything ends up matching everything after you've reached
the top of the hierarchy.  Try putting costs in your hierarchy, and give the matcher a 'budget',
and 'charge' to ascend the hierarchy. Or just omit the very top of the hierarchy.

A comment - large data sets of such hierarchies are available. If using this technique "for real" a
corpus like [https://wordnet.princeton.edu/](wordnet) is invaluable. If you want to play with your
solution and wordnet, a prolog version of wordnet is available from the same site.

A second approach is to rewrite parts of the sentence into simpler forms. "welding class" can
become just "class", or even `welding_class`. Adjectives add information, but aren't necessary. Definitely, you should
discard articles like "a" and "the". Prepositions like "to" in "to the cat"

Look up _Levenshtein distance_ . It can be applied treating each word as a letter. Levenshtein distance
is a relatively low computation way to find probable matches to work on later.

This problem is a 'toy' version of a problem that has consumed entire research careers. It has no perfect
answer (if you build a perfect solution to this problem, please contact the author!).

# 3) Ugraphs format (moderate, using library)

Given a knowledgebase of graph vertices and edges (contained in file graph_data.pl)

Figure out if the graph is _bipartite_. If you divide the vertices into two disjoint sets (say color them red
or blue), and there is a coloring such that every edge is between vertices of different colors, the graph
is bipartite.

You are explicitly encouraged to use the library [http://www.swi-prolog.org/pldoc/man?section=ugraphs](ugraphs)

# 4) Bipartite graphs (hard, good with #3, list manipulation)

Bipartite graphs are useful for assignment of resources problems.

take the bipartite graphs from #1 and assume that vertex 1, and all vertices the same color as 1, are resources,
like an industrial machine, and the other color vertices are jobs to be done. So maybe job 4 can be done
on machine 2 or machine 4.

Make a predicate that assigns one job to each resource.

# 5) Vocabulary (easy, lists, aggregates)

It would be convenient to have a list of words used in the data of problem 2.

Take the data from problem 2 and make a sorted list of all the words, with each word used once.

# 6) Choo Choo!  (moderate, lists)

The Hooterville railroad has a single track thar runs from Albany to Folsom. Here's the stations, and 
distances (in minutes travel) between them.

Albany - 25 - Baker - 10 - Carlson - 45 - Dexter - 10 - Eugene - 15 - Folsom. 

Trains coming from opposite directions can only pass at stations.

Once an hour a train starts from each end.

Write a simulator that 'runs' the trains.   Between each station, randomly add up to 50% time
to the nominal travel time. If the trains meet other than at a station, they crash.

Write a second module that can instruct trains to hold at stations until they meet another train.  
This module should be 'told' only the time and station name last passed. 
This module should prevent crashes, while minimizing time trains spend waiting.

# 7) Choo choo switching (moderate, lists)

Railroad yards have a number of tracks. The yard is the 'sorter' for cars. 

Albany yard has 4 tracks. There are cars for trains A,B,C, and D.

4 ABBAACCD
3 BACDAB
2 ACDCC
1 BBBAAA

The engine can take as many cars from the end as it wants and move them, in the same order, to 
the end of another track.

So, here's some moves that put all the B cars together.  We'll put them on track 1
since there's some there already.

```

1. move the A cars out of the way on track 2

4 ABBAACCD
3 BACDAB
2 ACDCCAAA
1 BBB

2. move all the cars from 3 to 1

4 ABBAACCD
3 
2 ACDCCAAA
1 BBBBACDAB

3. move cars ACDAB back to 3

4 ABBAACCD
3 ACDAB
2 ACDCCAAA
1 BBBB

4. move all but one car from 4 to 3

4 A
3 ACDABBBAACCD
2 ACDCCAAA
1 BBBB

5. move BBBAACCD from 3 to 1

4 A
3 ACDA
2 ACDCCAAA
1 BBBBBBBAACCD

6. move AACCD from 1 to 2

4 A
3 ACDA
2 ACDCCAAAAACCD
1 BBBBBBB

note that this last move also helps make train A

Make a program that prints out moves to sort out all the cars in the yard.

Test data:

[`abcdbababaaacccd`,
 `abcdaaabbbddddd`,
 ``,
 `aaaaaaabbbbbbbb`
]

[`bbbbbbcccdddd`,
`aaaaabbbbb`,
`aaaaaabbb`,
`abcdd`
]

[ ``,
``,
`aa`,
``
]

[`abbddaa`,
`ddccc`,
`cccaaa`,
`ccdab`
]

```

Hint - Prolog lists are easier to manipulate from the head end. Make the first car in the list be the car
on the **right** in these diagrams.





