
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(listing)).

:- dynamic(country_location/2).

latlong_from_google(Name,
	   loc( FA,
	       box(Bound_NELat, Bound_NELong, Bound_SWLat, Bound_SWLong),
	       pair(Loc_Lat, Loc_Long),
		box(View_NELat, View_NELong, View_SWLat, View_SWLong)
	      )) :-
	atom_codes(AName , Name),
	www_form_encode(AName , FEName),
	atom_concat('http://maps.googleapis.com/maps/api/geocode/json?address=',
		    FEName , PreReq),
	atom_concat(PreReq, '&sensor=false', Req),
	http_open(Req, Stream, []),
  % read_stream_to_codes(In , Codes),
    json_read(Stream , Term),
    close(Stream),
    json([results=Results,status='OK']) = Term,
   [json([address_components=_AC, formatted_address=FA,   geometry=GEO,types=_TYPES])] = Results,
   json(
     [ =(bounds,
         json(
              [ northeast=json([lat= Bound_NELat, lng=Bound_NELong]),
                southwest=json([lat= Bound_SWLat, lng=Bound_SWLong])
              ])),
       location=json([lat= Loc_Lat, lng=Loc_Long]),
       location_type=_,
       =(viewport,
         json(
              [ northeast=json([lat= View_NELat, lng=View_NELong]),
                southwest=json([lat= View_SWLat, lng=View_SWLong])
              ]))
     ]) = GEO.
 %  portray_clause(GEO),nl.

write_loc(loc( FA,
	       box(Bound_NELat, Bound_NELong, Bound_SWLat, Bound_SWLong),
	       pair(Loc_Lat, Loc_Long),
		box(View_NELat, View_NELong, View_SWLat, View_SWLong)
	      )) :-
 format('~w,~f,~f,~f,~f,~f,~f,~f,~f,~f,~f~n',
	[FA, Bound_NELat, Bound_NELong, Bound_SWLat, Bound_SWLong,
	 Loc_Lat, Loc_Long,
	 View_NELat, View_NELong, View_SWLat, View_SWLong]).


assert_capital_locations :-
	retractall(country_location(_,_)),
	open('countrynames.csv' , read , Stream),
	repeat,
	read_line_to_codes(Stream , Name),
	(   Name = end_of_file ->
	    true
	;
	    latlong_from_google(Name, Loc),
	    asserta(country_location(Name, Loc)),
	    write_loc(Loc),
	    fail
	),
	close(Stream).
