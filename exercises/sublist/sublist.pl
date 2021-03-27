lista([]).
lista([_|Y]) :- lista(Y).

sublist([],Y) :- lista(Y).
sublist([X|Xp],[X|Yp]) :-
    sublist1(Xp,Yp).
sublist(X,[_|Yp]) :-
    sublist(X,Yp).

sublist1([],Y) :- lista(Y).
sublist1([X|Xp], [X|Yp]) :-
    sublist1(Xp,Yp).
