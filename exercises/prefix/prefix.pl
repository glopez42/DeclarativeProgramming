lista([]).
lista([_|Y]):- lista(Y).

prefix([],Y) :- lista(Y).
prefix([X|XP], [X|YP]) :-
    prefix(XP,YP).



