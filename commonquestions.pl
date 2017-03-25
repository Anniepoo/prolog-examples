/*

         WTF?


This file answers common 'beginner' questions about Prolog

*/

/*
    How do I make an if statement?

    Prolog has no control structures. Well, it has some, but
    using them is not what you want to do.

    Lets say you have

 void say_hi_to_bob(X)
 {
    if(x == "Bob")
       writeln("Hi Bob!")
    else
       writeln("Sorry, you aren\'t Bob")
 }

   in your head

   The solution in Prolog is that these are two cases that can
   be handled by two clauses
    */
say_hi_to_bob(bob) :-
	writeln('Hi Bob!').
say_hi_to_bob(X) :-
	X \= bob,     % X will not unify with Bob
	writeln('Sorry, you aren\'t Bob').

/*
	say_hi_to_bob is 'steadfast'. If you backtrack into it,
	you'll be safe.

        Often it's ok to not be 'steadfast'. If you're sure
	you'll not backtrack into say_hi_to_bob, you can shorten
	it a bit.

        the second clause doesn't check if we're not bob,
	but isn't reached because the first clause succeeded.

*/
say_hi_to_bob(bob) :-
	writeln('Hi Bob!').
say_hi_to_bob(_) :-
	writeln('Sorry, you aren\'t Bob').

/*
       How do I write a loop in Prolog?

       there are two ways, depending on what you're doing

       The first way is to replace it with recursion. This
       is the normal way to step through lists

       See also loops.pl in this collection
*/
print_by_recursion([]).   % if the list is empty do nothing
print_by_recursion([H|T]) :-
	writeln(H),
	print_by_recursion(T).

/*
	 second way

	 failure driven loop
*/
print_members(List) :-
	member(X, List),
	writeln(X),
	fail.
print_members(_).

/*
	common variation of failure driven loop

	repeat always succeeds and leaves behind a choice point

	read reads the next term from the user. Run this, and type in
	stop. <newline> a few times, then go. <newline>

	how it works:
	the read can't match it's argument - the atom go
	unless what the user typed in was go.
        so read fails. The last choice point is at the repeat,
	which makes another choice point and succeeds

*/
wait_for_go :-
	repeat,
	write('can I go?'),
	flush,
	read(go).
/*
    what do you mean 4 = 2 + 2 fails!
    this will fail
*/
did_prolog_pass_second_grade :-
	4 = 2 + 2.
/*
    WTF?
    = is NOT 'evaluate the right side and compare to other side'
    instead = means 'can the two sides be unified?'
    But here, we're asking 'can the number 4 be unified with a complex
    term whose functor is '+'/2 and args are 2 and 2?'
    4 = '+'(2,2)
    It's not evaluating the right side. So of course these are
    different.
    If you want arithmetic, use 'is'
    4 is 2 + 2

    Why this insanity?
    Because it gives us something massively cool - terms are a
    fundamental data type. This lets us structure data
    X = employee('Bob Jones', '123 Main St', engineering_dept,
    software_engineer_2)
    and X is an employee record.
    on a more subtle level, it lets us treat code and data alike.

*/
