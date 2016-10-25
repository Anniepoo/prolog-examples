base_score(might, 11).
base_score(skill, 12).
base_score(wits, 16).
base_score(luck, 16).
base_score(will, 13).
base_score(grace, 11).

% Begin defining derived Predicates.
attribute(might).
attribute(wits).
attribute(skill).
attribute(luck).
attribute(will).
attribute(grace).

modifier(Attribute, Out_Mod) :-
  attribute(Attribute),
  base_score(Attribute, Score),
  modifier2(Score, Out_Mod).
modifier2(1, -4).
modifier2(2, -4).
modifier2(3, -3).
modifier2(4, -3).
modifier2(5, -2).
modifier2(6, -2).
modifier2(7, -1).
modifier2(8, -1).
modifier2(X, 0) :- X >= 9, X =< 12.
modifier2(X, 1) :- X >= 13, X =< 14.
modifier2(X, 2) :- X >= 15, X =< 16.
modifier2(X, 3) :- X >=17, X =< 18.
modifier2(X, 4) :- X >= 19, X =< 20.

:- discontiguous(score/2).
score(melee_score, Score) :- melee_score(Score).
melee_score(Score) :- 
  modifier(might, X), 
  modifier(skill, Y), 
  modifier(luck, Z), 
  Score is X + Y + Z, !.

score(missile_score, Score) :- missile_score(Score).
missile_score(Score) :- 
  modifier(skill, X),
  modifier(wits, Y),
  modifier(luck, Z),
  Score is X + Y + Z, !.

score(initiative_score, Score) :- initiative_score(Score).
initiative_score(Score) :-
  modifier(skill, X),
  modifier(luck, Y),
  Score is 10 + X + Y, !.

score(defense_class, Score) :- defense_class(Score).
defense_class(Score) :-
  modifier(luck, X),
  armor(Y),
  Score is 12 + X + Y.

score(hits_total, Score) :- hits_total(Score).
hits_total(Score) :-
  modifier(skill, Y),
  Score is 10 + Y.

score(effective_defense, Score) :- effective_defense(Score).
effective_defense(Score) :-
  armor(Armor),
  defense_class(Defense),
  modifier(wits, Wits),
  Score is Armor + Defense + Wits.

score(athletic_prowess, Score) :- athletic_prowess(Score).
athletic_prowess(Score) :-
  modifier(might, Might),
  modifier(skill, Skill),
  modifier(luck, Luck),
  Score is Might + Skill + Luck.

score(danger_evasion, Score) :- danger_evasion(Score).
danger_evasion(Score) :-
  modifier(wits, Wits),
  modifier(skill, Skill),
  modifier(luck, Luck),
  Score is Wits + Skill + Luck.

score(mystic_fort, Score) :- mystic_fort(Score).
mystic_fort(Score) :-
  modifier(will, Will),
  modifier(wits, Wits),
  modifier(luck, Luck),
  Score is Will + Wits + Luck.

score(physical_vigor, Score) :- physical_vigor(Score).
physical_vigor(Score) :-
  modifier(might, Might),
  modifier(will, Will),
  modifier(luck, Luck),
  Score is Might + Will + Luck.

score(charisma, Score) :- charisma(Score).
charisma(Score) :-
  modifier(will, Will),
  modifier(grace, Grace),
  modifier(luck, Luck),
  Score is Will + Grace + Luck.

score(thievery, Score) :- thievery(Score).
thievery(Score) :-
  modifier(wits, Wits),
  modifier(luck, Luck),
  Score is Wits + Luck.

score(armor, Score) :- armor(Score).
armor(Score) :-
    Score is 0.

d20_check(Type, Result) :-
  score(Type, Score),
  random_between(1, 20, Die),
  Result is Score + Die, 
    !,
    format("Result was ~p!", Result).

d20_check(Type, Result, Threshold) :-
    d20_check(Type, Result),
     (Threshold =< Result,
    format("~n Result successful."), !) |
    format("~n Roll failed."), !.

rollables(Bag) :- 
    findall(X, score(X, _), Bag).
