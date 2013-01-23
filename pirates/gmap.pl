:- module(gmap,
	  [ gmap//1			% +Coordinates
	  ]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/html_write)).
:- use_module(library(settings)).

:- setting(key, atom, 'AIzaSyD2BfdFtyGXcVtACmZ4tbmGLoUDV-m20RI',
	   'Google map key.  "abcdefg" works for localhost').
:- setting(script, atom, 'http://maps.google.com/maps?file=api&v=2&sensor=false',
	   'Address of Google map script').

%%	gmap(+Coordinates)// is det.
%
%	HTML component that shows Google maps  with markers at the given
%	Coordinates. Coordinates is a list. Each  coordinate is either a
%	term point(Lat,Long) or a URI with  the properties wgs84:lat and
%	wgs84:long.

gmap(Coordinates) -->
	google_script,
	html(div([ id(map_canvas),
		   style('width: 90%; height: 600px;')
		 ],
		 [])),
	show_map(Coordinates).

google_script -->
	{ setting(key, Key),
	  setting(script, Script),
	  format(atom(Src), '~w&key=~w', [Script, Key])
	},
	html(script([ src(Src),
		      type('text/javascript')
		    ],
		    [])).

show_map(Coordinates) -->
	{ avg(Coordinates, point(CLat, CLong))
	},
	html(script(type('text/javascript'),
		    [ 'if (GBrowserIsCompatible()) {\n',
		      'var map = new GMap2(document.getElementById("map_canvas"));\n',
		      'map.setCenter(new GLatLng(~w,~w), 2);\n'-[CLat, CLong],
		      \coords(Coordinates),
		      'map.setUIToDefault();\n',
		      '}\n'
		    ])).

coords([]) --> [].
coords([C|T]) -->
	{ coordinate(C, Lat, Long) },
	html('map.addOverlay(new GMarker(new GLatLng(~w,~w)));\n'-[Lat,Long]),
	coords(T).

avg([] , point(0.0, 0.0)) :- !.
avg(Coordinates, point(ALat, ALong)) :-
	sum_ll(Coordinates, 0, SumLat, 0, SumLong),
	length(Coordinates, Count),
	ALat is SumLat/Count,
	ALong is SumLong/Count.

sum_ll([], Lat, Lat, Long, Long).
sum_ll([C|T], Lat0, LatS, Long0, LongS) :-
	coordinate(C, Lat, Long),
	Lat1 is Lat0+Lat,
	Long1 is Long0+Long,
	sum_ll(T, Lat1, LatS, Long1, LongS).

coordinate(point(Lat, Long), Lat, Long).
coordinate(R, Lat, Long) :-
	rdf_has(R, wgs84:lat, LatLit),
	rdf_has(R, wgs84:long, LongLot),
	lit_number(LatLit, Lat),
	lit_number(LongLot, Long).

lit_number(literal(type(_T, Atom)), Number) :- !,
	   catch(atom_number(Atom, Number), _, fail).
lit_number(literal(Atom), Number) :-
	   catch(atom_number(Atom, Number), _, fail).
