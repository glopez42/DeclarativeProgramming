:- module(_,_,[assertions,regtypes]).

alumno_prode('Lopez','Garcia','Guillermo','a180182').


% nat/1, es cierto si la expresión es un número natural
nat(0).
nat(s(X)) :- nat(X).

% lista/1, es cierto si la expresión es una lista
lista([]).
lista([_|Y]):-
    lista(Y).

% plus/3, es cierto si el tercer argumento es la suma de los dos primeros
plus(X,0,X) :-
    nat(X).
plus(X,s(Y),s(Z)) :-
    plus(X,Y,Z).

% member/2, es cierto si el primer argumento está contenido en la lista
% del segundo argumento
esMiembro(X,[X|Y]) :- list(Y).
esMiembro(X,[_|T]) :-
    esMiembro(X,T).


% nums/2, es cierto si el primer argumento es un natural
% y el segundo una lista descendente desde ese número, hasta 1
nums(s(0),[s(0)]).
nums(s(N),[s(N)|Np]) :-
    nat(N),
    nums(N,Np).


% sumlist/2, es cierto si el segundo argumento es la suma de los
% elementos del primero
sumlist([],0).
sumlist([N|Np],S) :-
    plus(N,Sp,S), % es cierto si Sp = N - S
    sumlist(Np,Sp).


% choose_one/3, dado un elemento, devuelve cierto si el tercer argumento
% es una lista igual a la del segundo, pero sin ese elemento
choose_one(E,[E|Lp],Lp) :- % caso base, si se ha encontrado E y el resto es lo mismo
    lista(Lp).
choose_one(E,[X|Lp],[X|Rp]) :-
    choose_one(E,Lp,Rp).


% perm/3, cierto si la segunda lista es una permutación de la primera
perm([],[]). % caso base, dos listas vacías
perm([X|L],Lp) :-
    choose_one(X,Lp,R), % se quita X de Lp si se encuentra, y se guarda en R
    perm(L,R). % se comprueba con lo que queda de las listas




    