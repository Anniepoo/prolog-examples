% sudoku.pl
% Aswin F. van Woudenberg

:- use_module(library('clp/bounds')).

sudoku([
		AA, AB, AC, AD, AE, AF, AG, AH, AI,
		BA, BB, BC, BD, BE, BF, BG, BH, BI,
		CA, CB, CC, CD, CE, CF, CG, CH, CI,
		DA, DB, DC, DD, DE, DF, DG, DH, DI,
		EA, EB, EC, ED, EE, EF, EG, EH, EI,
		FA, FB, FC, FD, FE, FF, FG, FH, FI,
		GA, GB, GC, GD, GE, GF, GG, GH, GI,
		HA, HB, HC, HD, HE, HF, HG, HH, HI,
		IA, IB, IC, ID, IE, IF, IG, IH, II]) :-
	Vars = [
		AA, AB, AC, AD, AE, AF, AG, AH, AI,
		BA, BB, BC, BD, BE, BF, BG, BH, BI,
		CA, CB, CC, CD, CE, CF, CG, CH, CI,
		DA, DB, DC, DD, DE, DF, DG, DH, DI,
		EA, EB, EC, ED, EE, EF, EG, EH, EI,
		FA, FB, FC, FD, FE, FF, FG, FH, FI,
		GA, GB, GC, GD, GE, GF, GG, GH, GI,
		HA, HB, HC, HD, HE, HF, HG, HH, HI,
		IA, IB, IC, ID, IE, IF, IG, IH, II],
	Vars in 1..9,
	all_different([AA, AB, AC, BA, BB, BC, CA, CB, CC]),
	all_different([AD, AE, AF, BD, BE, BF, CD, CE, CF]),
	all_different([AG, AH, AI, BG, BH, BI, CG, CH, CI]),
	all_different([DA, DB, DC, EA, EB, EC, FA, FB, FC]),
	all_different([DD, DE, DF, ED, EE, EF, FD, FE, FF]),
	all_different([DG, DH, DI, EG, EH, EI, FG, FH, FI]),
	all_different([GA, GB, GC, HA, HB, HC, IA, IB, IC]),
	all_different([GD, GE, GF, HD, HE, HF, ID, IE, IF]),
	all_different([GG, GH, GI, HG, HH, HI, IG, IH, II]),
	all_different([AA, AB, AC, AD, AE, AF, AG, AH, AI]),
	all_different([BA, BB, BC, BD, BE, BF, BG, BH, BI]),
	all_different([CA, CB, CC, CD, CE, CF, CG, CH, CI]),
	all_different([DA, DB, DC, DD, DE, DF, DG, DH, DI]),
	all_different([EA, EB, EC, ED, EE, EF, EG, EH, EI]),
	all_different([FA, FB, FC, FD, FE, FF, FG, FH, FI]),
	all_different([GA, GB, GC, GD, GE, GF, GG, GH, GI]),
	all_different([HA, HB, HC, HD, HE, HF, HG, HH, HI]),
	all_different([IA, IB, IC, ID, IE, IF, IG, IH, II]),
	all_different([AA, BA, CA, DA, EA, FA, GA, HA, IA]),
	all_different([AB, BB, CB, DB, EB, FB, GB, HB, IB]),
	all_different([AC, BC, CC, DC, EC, FC, GC, HC, IC]),
	all_different([AD, BD, CD, DD, ED, FD, GD, HD, ID]),
	all_different([AE, BE, CE, DE, EE, FE, GE, HE, IE]),
	all_different([AF, BF, CF, DF, EF, FF, GF, HF, IF]),
	all_different([AG, BG, CG, DG, EG, FG, GG, HG, IG]),
	all_different([AH, BH, CH, DH, EH, FH, GH, HH, IH]),
	all_different([AI, BI, CI, DI, EI, FI, GI, HI, II]),
	label(Vars).

write_sol([
		AA, AB, AC, AD, AE, AF, AG, AH, AI,
		BA, BB, BC, BD, BE, BF, BG, BH, BI,
		CA, CB, CC, CD, CE, CF, CG, CH, CI,
		DA, DB, DC, DD, DE, DF, DG, DH, DI,
		EA, EB, EC, ED, EE, EF, EG, EH, EI,
		FA, FB, FC, FD, FE, FF, FG, FH, FI,
		GA, GB, GC, GD, GE, GF, GG, GH, GI,
		HA, HB, HC, HD, HE, HF, HG, HH, HI,
		IA, IB, IC, ID, IE, IF, IG, IH, II]) :-
	write('+---+---+---+'), nl,
	write('|'), write(AA), write(AB), write(AC), write('|'),write(AD), write(AE), write(AF), write('|'),write(AG), write(AH), write(AI), write('|'),nl,
	write('|'), write(BA), write(BB), write(BC), write('|'),write(BD), write(BE), write(BF), write('|'),write(BG), write(BH), write(BI), write('|'),nl,
	write('|'), write(CA), write(CB), write(CC), write('|'),write(CD), write(CE), write(CF), write('|'),write(CG), write(CH), write(CI), write('|'),nl,
	write('+---+---+---+'), nl,
	write('|'), write(DA), write(DB), write(DC), write('|'),write(DD), write(DE), write(DF), write('|'),write(DG), write(DH), write(DI), write('|'),nl,
	write('|'), write(EA), write(EB), write(EC), write('|'),write(ED), write(EE), write(EF), write('|'),write(EG), write(EH), write(EI), write('|'),nl,
	write('|'), write(FA), write(FB), write(FC), write('|'),write(FD), write(FE), write(FF), write('|'),write(FG), write(FH), write(FI), write('|'),nl,
	write('+---+---+---+'), nl,
	write('|'), write(GA), write(GB), write(GC), write('|'),write(GD), write(GE), write(GF), write('|'),write(GG), write(GH), write(GI), write('|'),nl,
	write('|'), write(HA), write(HB), write(HC), write('|'),write(HD), write(HE), write(HF), write('|'),write(HG), write(HH), write(HI), write('|'),nl,
	write('|'), write(IA), write(IB), write(IC), write('|'),write(ID), write(IE), write(IF), write('|'),write(IG), write(IH), write(II), write('|'),nl,
	write('+---+---+---+'), nl.
	
get_num(NumValue) :-
	get_single_char(Value),
	(member(Value, [49,50,51,52,53,54,55,56,57]),
	 NumValue is Value - 48,
	 write(NumValue)), !;
	(write(' ')).

read_sudoku([
		AA, AB, AC, AD, AE, AF, AG, AH, AI,
		BA, BB, BC, BD, BE, BF, BG, BH, BI,
		CA, CB, CC, CD, CE, CF, CG, CH, CI,
		DA, DB, DC, DD, DE, DF, DG, DH, DI,
		EA, EB, EC, ED, EE, EF, EG, EH, EI,
		FA, FB, FC, FD, FE, FF, FG, FH, FI,
		GA, GB, GC, GD, GE, GF, GG, GH, GI,
		HA, HB, HC, HD, HE, HF, HG, HH, HI,
		IA, IB, IC, ID, IE, IF, IG, IH, II]) :-
	write('+---+---+---+'), nl,
	write('|'), get_num(AA), get_num(AB), get_num(AC), write('|'),get_num(AD), get_num(AE), get_num(AF), write('|'),get_num(AG), get_num(AH), get_num(AI), write('|'),nl,
	write('|'), get_num(BA), get_num(BB), get_num(BC), write('|'),get_num(BD), get_num(BE), get_num(BF), write('|'),get_num(BG), get_num(BH), get_num(BI), write('|'),nl,
	write('|'), get_num(CA), get_num(CB), get_num(CC), write('|'),get_num(CD), get_num(CE), get_num(CF), write('|'),get_num(CG), get_num(CH), get_num(CI), write('|'),nl,
	write('+---+---+---+'), nl,
	write('|'), get_num(DA), get_num(DB), get_num(DC), write('|'),get_num(DD), get_num(DE), get_num(DF), write('|'),get_num(DG), get_num(DH), get_num(DI), write('|'),nl,
	write('|'), get_num(EA), get_num(EB), get_num(EC), write('|'),get_num(ED), get_num(EE), get_num(EF), write('|'),get_num(EG), get_num(EH), get_num(EI), write('|'),nl,
	write('|'), get_num(FA), get_num(FB), get_num(FC), write('|'),get_num(FD), get_num(FE), get_num(FF), write('|'),get_num(FG), get_num(FH), get_num(FI), write('|'),nl,
	write('+---+---+---+'), nl,
	write('|'), get_num(GA), get_num(GB), get_num(GC), write('|'),get_num(GD), get_num(GE), get_num(GF), write('|'),get_num(GG), get_num(GH), get_num(GI), write('|'),nl,
	write('|'), get_num(HA), get_num(HB), get_num(HC), write('|'),get_num(HD), get_num(HE), get_num(HF), write('|'),get_num(HG), get_num(HH), get_num(HI), write('|'),nl,
	write('|'), get_num(IA), get_num(IB), get_num(IC), write('|'),get_num(ID), get_num(IE), get_num(IF), write('|'),get_num(IG), get_num(IH), get_num(II), write('|'),nl,
	write('+---+---+---+'), nl.

solve_sudoku :-
	read_sudoku(L), 
	sudoku(L),
	write_sol(L).
