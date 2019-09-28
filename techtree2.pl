/* This example solves questions about
 *  "tech trees" using Constraint Handling Rules
 *
 *  Tech Trees are a common game mechanic where the player,
 *  or AI opponent, must build a specific building or unit
 *  before they can build another.
 *  Seeing a unit, the opposing player can then infer that
 *  their opponent possesses all the units needed to
 *  So, for example, if building tanks requires the tank factory,
 *  and the tank factory requires the foundry, then if we see a tank
 *  we can infer they have the foundry.
 *
 *  Our logic doesn't take into account later destruction of units.
 *
 *  Usage is slightly complicated by the fact that returning to the
 *  top level clears the constraint store. Hence provision of the i_saw
 *  convenience predicate.
 *
 *  CHR syntax, since I can never remember it:
 *  name @ retained \ discarded <=> guard | head,body.    Simpagation
 *  name @ discarded <=> guard | head, body.      Simplification
 *  name @ retained ==> guard | head, body.         Propagation
 *
 *  Usage:
 *  ?- init, i_saw(boat).
 *  ?- init, i_saw([boat, knight]).
 *
 *  stepping in the chr_debugger is useful
 *
 *  ?- chr_trace,init,i_saw(boat).
 *
 *  useful references
 *  https://dtai.cs.kuleuven.be/CHR/files/tutorial_iclp2008.pdf
 *  https://dtai.cs.kuleuven.be/CHR/
 *  https://dtai.cs.kuleuven.be/CHR/webchr.shtml
 *  https://www.swi-prolog.org/pldoc/man?section=chr
 *
 *  Copyright (c) 2019   Anne Ogborn
 *  released under the accompanying MIT license
 */


:- use_module(library(chr)).

% CHR constraints must be defined using this directive
:- chr_constraint
    saw/1, % I saw a unit of this type
    has/1, % I can infer enemy has a unit of this type
    can_build/1, % I can infer enemy can build a unit of this type
    can_build_if/2, % enemy can build a unit of this type if arg2 list all exist
    needs/2,     % game rule, to build arg1, all of list arg2 must exist
    reset/0.     % reset the game world

% reset the game elements
reset \ saw(_) <=> true.
reset \ has(_) <=> true.
reset \ can_build(_) <=> true.
reset \ needs(_, _) <=>true.
reset \ can_build_if(_, _) <=> true.
reset <=> true.

%
% set the initial state of the constraint store,
% with the game dependencies and two initial peasants
%
% to make barracks must have peasant, etc.
init :-
    needs(barracks, [peasant]),
    needs(stable, [peasant, barracks]),
    needs(dock, [peasant, barracks]),
    needs(swordsman, [barracks]),
    needs(knight, [stable]),
    needs(boat, [dock, peasant]),
    saw(peasant),  % we 'saw' the peasant
    saw(peasant).  % because game starts with 2 peasants


% enforce set semantics for various things
% once we know they have a boat, we don't want to add
% that again
saw(X), saw(X) <=> saw(X).
has(X), has(X) <=> has(X).
can_build(X), can_build(X) <=> can_build(X).
can_build_if(X, Y), can_build_if(X, Y) <=> can_build_if(X, Y).

% common sense
saw(X) ==> has(X).         % I saw it, they have it
has(X) ==> can_build(X).   % they have it, they can make it

% this only exists briefly
:- chr_constraint must_have/1.

% expresses the idea 'they have tanks, they must have a tank factory'
% because needs has a list, we recursively fire the must_have rule
% to add everything
has(X), needs(X, List) ==> must_have(List).
must_have([]) <=> true.
must_have([X|Y]) <=> has(X), must_have(Y).

% add can_build for everything whose needs exist
% having dock and peasant adds can_build(boat).
% we wait until the first element of list exists,
% then go on to second element and wait, and so on
needs(X, Z) ==> can_build_if(X, Z).
can_build_if(X, []) <=> can_build(X).
has(Y), can_build_if(X, [Y | Z]) <=> can_build_if(X, Z), has(Y).

% convenience prolog predicate for testing.
% pass list of units seen
i_saw(X) :-
    atomic(X),
    call(saw(X)),
    print_store.
i_saw(X) :-
    is_list(X),
    maplist(callsaw , X),
    print_store.

callsaw(X) :- call(saw(X)).

%
% print out results of computation, showing
% what the enemy has built, and what they can build
%
print_store :-
    writeln('==============='),
    find_chr_constraint(has(Y)),
    format('Your enemy has built ~w~n', [Y]),
    fail.
print_store :-
    nl,
    find_chr_constraint(can_build(Y)),
    format('Your enemy can build ~w~n', [Y]),
    fail.
print_store.

		 /*******************************
		 *     helpful utilities        *
		 *******************************/

% print out the constraint store
ps :-
    find_chr_constraint(Y),
    format('constraint store contains ~w~n', [Y]),
    fail.
ps.

% print out constraint store when you return to top level
ss :- set_prolog_flag(chr_toplevel_show_store, true).

% or don't
noss :- set_prolog_flag(chr_toplevel_show_store, false).




