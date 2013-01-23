:- module(demo,
          [ event/1,		% ?Event
	    event_point/2,
	    event_in_box/5,
	    event_actor_type/2,
	    passenger_broken/1,
	    passenger_works/1
          ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

%%	event(?Event)
%
%	True if Event is an event in the Simple Event Model.

event(Event) :-
        rdfs_individual_of(Event, sem:'Event').

%%	event_point(?Event, ?Point)
%
%	True if Event happened at point(Lat,Lon),   where  Lat and Lon
%	are the latitute and longitude of the event in decial degrees.

event_point(Event, point(Lat, Lon)) :-
        event(Event),
        rdf_has(Event, sem:hasPlace, Place),
        rdf_has(Place, wgs84:lat, literal(type(xsd:decimal, LatText))),
        rdf_has(Place, wgs84:long, literal(type(xsd:decimal, LonText))),
        atom_number(LatText, Lat),
        atom_number(LonText, Lon).

event_in_box(Event, Lat1, Lon1, Lat2, Lon2) :-
	event_point(Event, point(Lat, Lon)),
	Lat >= Lat1,
	Lat =< Lat2,
	Lon >= Lon1,
	Lon =< Lon2.

:- rdf_meta event_actor_type(r, r).

event_actor_type(Event, Type) :-
	event(Event),
	rdf_has(Event, sem:hasActor, Victim),
	rdf_has(Victim, sem:actorType, VictimType),
	rdf_reachable(Type, sem:hasSubType, VictimType).

:- rdf_meta passenger_broken(r).

passenger_broken(Event) :-
	event_actor_type(Event, poseidon:atype_passenger_vessel).

:- rdf_meta passenger_works(r).

passenger_works(Event) :-
         event_actor_type(Event,
    'http://semanticweb.cs.vu.nl/poseidon/ns/instances/atype_passenger_vessel').

