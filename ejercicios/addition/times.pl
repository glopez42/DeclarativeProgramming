nat(0).
nat(s(X)) :- nat(X).

plus(X,0,X) :- nat(X).
plus(X,s(Y),s(Z)) :- plus(X,Y,Z).
