nat(0).
nat(s(X)) :- nat(X).

plus(X,0,X) :- nat(X).
plus(X,s(Y),s(Z)) :- plus(X,Y,Z).
 
times(X,0,0):- nat(X).
times(X,s(Y),Z):-
    plus(W,X,Z),
    times(X,Y,W).