/*
 * This software is the property of The Elgin Works, LLC
 * Copyright (c) 2015 The Elgin Works
 *
 * This software is licensed under the terms of the MIT license,
 * a copy of which should have come with this software
 *
 */

:- module(think, []).
/** <module> Main handler for responding to requests to think
 *
 */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_json)).
:- use_module(library(chr)).

:- http_handler(public(think), think, []).

:- chr_option(debug, on).

think(Request) :-
	http_read_json_dict(Request, Query),
	solve_and_reset(Query, Solution),
	reply_json_dict(Solution).

solve_and_reset(Query, Solution) :-
	solve(Query, Solution),
	nb_setval(solution, Solution),
	fail.
solve_and_reset(_, Solution) :-
	nb_getval(solution, Solution).

solve(Query, _{ actions: Solution,
		success: Success}) :-
	add_current_conditions(Query),
	with_output_to(string(S), chr_show_store(think)),
	debug(think, '~w', [S]),
	(   collect_action(Solution),
	    Success  = true
	;   Success = false
	),
	!.

:- chr_constraint
	temp/1,
	vent/1,
	soil/1,
	heater/1,
	too_hot/0,
	too_cold/0,
	temp_ok/0,
	temp_in_range/0,
	temp_out_range/0,
	soil_is_dry/0,
	soil_is_wet/0,
	soil_ok/0,
	vent_closed/0,
	vent_open/0,
	vent_in_motion/0,
	heater_on/0,
	heater_off/0,
	todo_water/0,
	cmd/2,
	oops/0,
	collect/1.

add_current_conditions(Query) :-
	_{temp: Temp,
	  vent: Vent,
	  soil: Soil,
	  heater: Heat} :< Query,
	  todo_water,
	  temp(Temp),
	  vent(Vent),
	  soil(Soil),
	  heater(Heat).
collect_action(Solution) :-
	collect(Solution).

temp(T) ==> T > 60, T =< 80 | temp_in_range.
temp(T) ==> T =< 60 | temp_out_range.
temp(T) ==> T > 80 | temp_out_range.
temp(T) <=> T > 80 | too_hot.
temp(T) <=> T < 40 | too_cold.
temp(_) <=> temp_ok.

vent(0) <=> vent_closed.
vent(255) <=> vent_open.
vent(_) <=> vent_in_motion.

soil(X) <=> X < 20 | soil_is_dry.
soil(X) <=> X > 80 | soil_is_wet.
soil(_) <=> soil_ok.

heater(1) <=> heater_on.
heater(0) <=> heater_off.

too_hot \ heater_on  <=> cmd(heater, off).
too_cold \ heater_off  <=> cmd(heater, on).

too_hot \ vent_closed <=> cmd(vent, open).
too_cold \ vent_open <=> cmd(vent, close).

temp_ok, soil_is_wet, vent_closed <=> cmd(vent, open).
temp_ok, soil_is_dry, vent_open <=> cmd(vent, close).

temp_ok, soil_is_dry \ heater_on <=> cmd(heater, off).

soil_is_wet \ todo_water <=> cmd(water, off).
soil_is_dry \ todo_water <=> cmd(water, on).
soil_ok, temp_in_range \ todo_water <=> cmd(water, on).
soil_ok, temp_out_range \ todo_water <=> cmd(water, off).

cmd(heater, off), cmd(heater, on) <=> oops.
cmd(vent, open), cmd(vent, close) <=> oops.

cmd(X, Y), cmd(X, Y) <=> cmd(X, Y).

collect(_), oops <=> fail.
collect(L), cmd(Unit, Value) <=> format(atom(A), '~w:~w', [Unit, Value]), L = [A | N], collect(N).
collect(X) <=> X = [], true.
