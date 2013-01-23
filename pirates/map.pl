:- module(map, []).
                                % Web-server libraries
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
                                % Pirates package Google map interface
:- use_module(components(gmap)).
                                % Import our query predicates
:- use_module(demo).

% Bind /map to the predicate map/1

:- http_handler('/map', map, []).

% The implementation of map/1

map(Request) :-
        http_parameters(Request,
                        [ lat1(Lat1, [float]),
                          lon1(Lon1, [float]),
                          lat2(Lat2, [float]),
                          lon2(Lon2, [float]),
			  type(Type, [])
                        ]),
	all_points_in_box(Lat1, Lon1, Lat2, Lon2, Type, Points),

        reply_html_page(title('Piracy events'),
                        [ h1('Piracy'), \gmap(Points)
                        ]).

/*
points_in_box(Point, Lat1, Lon1, Lat2, Lon2) :-
	event_in_box(Event, Lat1, Lon1, Lat2, Lon2),
        event_point(Event, Point).
*/

all_points_in_box(Lat1, Lon1, Lat2, Lon2, Type, Points) :-
        bagof(Point, points_in_box(Point, Lat1, Lon1, Lat2, Lon2, Type) , Points),
	!.
all_points_in_box(_, _, _, _, _, []).

points_in_box(Point, Lat1, Lon1, Lat2, Lon2, Type) :-
	event_in_box(Event, Lat1, Lon1, Lat2, Lon2),
        event_point(Event, Point),
	rdf_global_id(poseidon:Type, GlobalType),
	event_actor_type(Event, X),
	X = GlobalType.
