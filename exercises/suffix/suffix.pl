suffix([],[]).
suffix([X|Xp],[X|Yp]) :-
    suffix1(Xp,Yp).
suffix(Xp,[_|Yp]) :-
    suffix(Xp,Yp).

suffix1([],[]).
suffix1([X|Xp],[X|Yp]) :-
    suffix1(Xp,Yp).
