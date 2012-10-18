/*
     tic tac toe playing program
     you are x's and move first.
     to play, enter location on board

     board is printed like this
     x2o
     4ox
     78x
'
*/
/* a little wrapper to make starting a game convenient */
tictactoe :- game([b,b,b,b,b,b,b,b,b], x).

/* the core recursive call to play the game */
game(Board , _) :- win(x, Board),write('you win!').
game(Board , _) :- win(o, Board),write('I win!').
game(Board , _) :- cat(Board).   /* no more moves - not technically cat */
game(Board , x) :- moveUser(Board , NewBoard) , game(NewBoard , o).
game(Board , o) :- moveComputer(Board , NewBoard), game(NewBoard , x).

moveUser(Board , NewBoard) :- showBoard(Board), put('*'),get_char(Y), get_char(_), name(Y , [X]),
    Loc is X - 48, !,procUserMove(Loc,Board,NewBoard).

procUserMove(Loc,Board,NewBoard) :- Loc > 0, Loc < 10, playAt(x , Loc , Board , NewBoard).
/* this handles the invalid case */
procUserMove(_,Board,_) :- write('invalid move'),!, game(Board,x).

/* lists must be same length tim- these were getting triggered, how?
playAt(_ , _ , [_|_] , []) :- !,fail.
playAt(_ , _ , [] , [_|_]) :- !,fail. */
playAt(Player , 1 , [b|Tail] , [Player|Tail]).
playAt(Player , Loc , [H|TBoard] , [H|TNewBoard]) :-
   Loc > 1,
   M is Loc - 1 ,
   playAt(Player , M , TBoard , TNewBoard).

showBoard([]).
showBoard([H|T]) :- put(H),showBoard1(T).
showBoard1([H|T]) :- put(H),showBoard2(T).
showBoard2([H|T]) :- put(H),nl,showBoard(T).

win(A , [A,A,A,_,_,_,_,_,_]).
win(A , [_,_,_,A,A,A,_,_,_]).
win(A , [_,_,_,_,_,_,A,A,A]).

win(A , [A,_,_,A,_,_,A,_,_]).
win(A , [_,A,_,_,A,_,_,A,_]).
win(A , [_,_,A,_,_,A,_,_,A]).

win(A , [A,_,_,_,A,_,_,_,A]).
win(A , [_,_,A,_,A,_,A,_,_]).

cat([]).
cat([H|T]) :- H \= b, cat(T).

/* What winning board can we make by playing at or beyond Loc? */
playInstaWin(Loc , Board , NewBoard) :- Loc < 10 , playAt(o , Loc , Board , NewBoard) , win(o , NewBoard).
playInstaWin(Loc , Board , NewBoard) :- Loc < 9 , M is Loc + 1 , playInstaWin(M , Board , NewBoard).

/* N is the number of threats player Player has in this board */
countThreats(Player , Board , N) :- countThreatsFamulus(Player , Board , 0 , N , [
[a,a,a,@,@,@,@,@,@],
[@,@,@,a,a,a,@,@,@],
[@,@,@,@,@,@,a,a,a],
[a,@,@,a,@,@,a,@,@],
[@,a,@,@,a,@,@,a,@],
[@,@,a,@,@,a,@,@,a],
[a,@,@,@,a,@,@,@,a],
[@,@,a,@,a,@,a,@,@]]).

countThreatsFamulus(_ , _ , N , N , []).
countThreatsFamulus(Player , Board , SoFar , N , [HThreat|TThreat]) :-
    threatp(Player , Board , HThreat),
    M is SoFar + 1 ,
    countThreatsFamulus(Player , Board , M , N , TThreat).
countThreatsFamulus(Player , Board , SoFar , N , [_|TThreat]) :-
    countThreatsFamulus(Player , Board , SoFar , N , TThreat).

/* is this player threatening to win using this pattern?
a threat is having two of the 3 required elements filled in, and the remaining one blank.
so threatp(x , [x,b,x,o,o,b,x,o,b], [a,a,a,@,@,@,@,@,@]) succeeds.
Note that my implementation also calls a win position a threat */
threatp(_ , [], []) :- !,fail.
threatp(Player , [Player|TBoard] , [a|TPattern]) :- direthreatp(Player , TBoard , TPattern).
threatp(Player , [b|TBoard] , [a|TPattern]) :- threatp(Player , TBoard , TPattern).
threatp(Player , [_|TBoard] , [@|TPattern]) :- threatp(Player , TBoard , TPattern).

direthreatp(_ , [], []) :- !,fail.
direthreatp(Player , [Player|_] , [a|_]).
direthreatp(Player , [b|TBoard] , [a|TPattern]) :- direthreatp(Player , TBoard , TPattern).
direthreatp(Player , [_|TBoard] , [@|TPattern]) :- direthreatp(Player , TBoard , TPattern).

/***** end of countThreats related stuff *******/

/* where would o be forced to play, if we start looking at Loc and
Board is the remaining chunk of board? */
playForcedMove(Loc , _ , _) :- Loc > 9,!,fail.
playForcedMove(Loc , Board , NewBoard) :-
	playAt(o , Loc , Board , NewBoard) ,
	countThreats(x , Board , N) ,
	countThreats(x , NewBoard , M),
	M \= N.
playForcedMove(Loc , Board , NewBoard) :- Next is Loc + 1, playForcedMove(Next , Board , NewBoard).


/* play where we can make two or more threats */
playDualThreat(Loc , _ , _) :- Loc > 9,!,fail.
playDualThreat(Loc , Board , NewBoard) :-
	playAt(o , Loc , Board , NewBoard),
	countThreats(o , NewBoard , NThreats),
	NThreats >= 2.
playDualThreat(Loc , Board , NewBoard) :-
	Next is Loc + 1,
	playDualThreat(Next , Board , NewBoard).

/* play anyplace we can find a threat to make */
playThreat(Loc , _ , _) :- Loc > 9,!,fail.
playThreat(Loc , Board , NewBoard) :-
	playAt(o , Loc , Board , NewBoard),
	countThreats(o , NewBoard , NThreats),
	NThreats >= 1.
playThreat(Loc , Board , NewBoard) :-
	Next is Loc + 1,
	playThreat(Next , Board , NewBoard).

/* play anyplace we can */
playRandom(Loc , _ , _) :- Loc > 9,!,fail.
playRandom(Loc , Board , NewBoard) :-
	playAt(o , Loc , Board , NewBoard).
playRandom(Loc , Board , NewBoard) :-
	Next is Loc + 1,
	playRandom(Next , Board , NewBoard).

/* Tim - can I put these in a list - playInstaWin, etc? and DRY? */

/* if we can win this turn, do it */
moveComputer(Board , NewBoard) :- playInstaWin(1, Board , NewBoard).

/* if we must play somewhere to avoid losing, do it */
moveComputer(Board , NewBoard) :- playForcedMove(1 , Board , NewBoard).

/* otherwise play where we can make two or more threats */
moveComputer(Board , NewBoard) :- playDualThreat(1 , Board , NewBoard).

/* otherwise play where we can make a single threat */
moveComputer(Board , NewBoard) :- playThreat(1 , Board , NewBoard).

/* otherwise play anywhere */
moveComputer(Board , NewBoard) :- playRandom(1 , Board , NewBoard).















