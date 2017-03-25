%
% This is to help beginning Prolog programmers understand cuts
% by means of a little story
%
% This isn't the whole story of cuts. It just tries to help you 'grok'
% what 'cut' does, and what 'cut, fail' does. 

%
% You're a non fiction writer researching an event that happened in a
% park in the small town of Littletown several decades ago: the
% invention of the flying toaster.
%
% You've gotten a chance to drive to Littletown for a couple days of
% research.
% Now you want to find 'the park'. The existing accounts are pretty
% vague about where it is.
% Driving around you don't see anything that's obviously 'the park'
% but find several spots that might be 'the park' - a vacant lot,
% the wide lawns around the city hall, etc.
%
% Let's model the process of finding 'the park'.

%  Some locations
%  The first clause means city_hall is a location.
location(city_hall).
location(vacant_lot).
location(old_mill).
location(field).
location(barber_shop).

%
% Places that are flat and grassy might be 'the park'.

flat(city_hall).
flat(vacant_lot).
flat(field).

grassy(city_hall).
grassy(vacant_lot).


%
% places that might be the park
possibly_park(X) :-
	location(X),
	flat(X),
	grassy(X).

%
% Now you can query where the park might be like so:
% 	?- possibly_park(X).
% and you get some possible locations.

%
% Now suppose you turn a corner and find a park,
% with swing sets, picnic tables, barbeque grills,
% and a big sign that says 'Littletown Park. On this
% site the flying toaster was invented in 1987 by
% A Random Programmer.
%
% After you find this, it's obvious that 'the park' isn't
% the vacant lot or city hall.
% Let's make a new query that handles this.
%
% We'll define someplace as definitely the park.
definitely_park(littletown_park).

% and make a query that finds a set of best answers
% for the park's location
%
% this clause says 'if we have found the park definitely,
% ignore the possibly_park's  -
park(X) :-
	definitely_park(X), % this is the park
	!. % ignore all other solutions to park

%
% this clause says what happens otherwise
park(X) :-
	possibly_park(X).

%
% Run this query:
% 	?- park(X).
% and you'll see that there's only one answer. Comment out
% the definitely_park fact and you'll see all the possibles.


%
% Great - so, lets change the story again.
% Instead of finding (what is definitely) the park, 
% you get tired of driving around and stop at a store
% to ask for directions to the park. 
% The fellow in the store says that they used the park land to
% build a new city auditorium in 1995.
%
% So let's build a park2 that handles this situation.

park_gone.


park2(_) :-         % _ because we won't care what location is passed
	park_gone,  % if the park is gone
	!,          % ignore other solutions
	fail.       % but this isn't a solution either

% remainder like park
park2(X) :-
	definitely_park(X),
	!.

park2(X) :-
	possibly_park(X).

% Run the query:
% 	?- park2(X).
%
% and you get false. Comment out park_gone, and you get littletown_park.
% If you comment out definitely_park, you'll get all the possibles.
%

