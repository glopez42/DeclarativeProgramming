:- module(_,_,[assertions,regtypes]).

alumno_prode('Lopez','Garcia','Guillermo','a180182').

% PARTE 1

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


% nums/2, es cierto si el primer argumento es un natural
% y el segundo una lista descendente desde ese número, hasta 1
nums(0,[]).
nums(s(N),[s(N)|Np]) :-
    nums(N,Np).


% sumlist/2, es cierto si el segundo argumento es la suma de los
% elementos del primero
sumlist([],0).
sumlist([N|Np],S) :-
    sumlist(Np,Sp),  % se llega recursivamente al caso base
    plus(N,Sp,S).    % se suman los valores recursivamente


% choose_one/3, dado un elemento, devuelve cierto si el tercer argumento
% es una lista igual a la del segundo, pero sin ese elemento
choose_one(E,[E|Lp],Lp) :- % caso base, si se ha encontrado E y el resto es lo mismo
    lista(Lp).
choose_one(E,[X|Lp],[X|Rp]) :-
    choose_one(E,Lp,Rp).


% perm/3, cierto si la segunda lista es una permutación de la primera
perm([],[]). % caso base, dos listas vacías
perm([X|R],L) :-
    perm(R,Lp), % de este modo llega al último elemento recursivamente
    choose_one(X,L,Lp).  % con las repeticiones se va creando L



% split/3, cierto si la segunda lista tiene los elementos en posición par
% de la primera lista, y la tercera, los de posición impar
split([],[],[]). %caso base, las tres listas están vacías
split([X1,X2|Xn],[X1|Xp],[X2|Xpp]) :- % el primer argumento asegura que tenga un nº par de elementos
    split(Xn,Xp,Xpp).


sumlists(N,L1,L2,S) :-
    nums(N,L),        % L es una lista desde N a 1
    perm(L,Lp),       % Lp tiene que ser una permutación de L
    split(Lp,L1,L2),  % Lp es una composición de L1 y L2
    sumlist(L1,S),    % S es el resultado de la suma de elementos de L1 y L2
    sumlist(L2,S).



% PARTE 2

% make_matrix/3, cierto si el primer argumento(una lista), forma las filas de
% una matriz cuadrada de N*N
make_matrix([], _, []).
make_matrix(Lista, N, [Fila|Filas]):-
  take_N(Lista, N, Fila, Rest),  % se cogen los N elementos de Lista y se guardan en Fila
  make_matrix(Rest, N, Filas).   % se hace lo mismo con el resto de la lista

% take_N/4, cierto si el último argumento es una lista resultada de quitarle
% N elementos a la primera lista. Rest es lo que queda de la lista original
take_N(Rest, 0, [], Rest).
take_N([Elem|Lista], s(N), [Elem|Lista2], Rest):-
  take_N(Lista, N, Lista2, Rest).

% check_sum/2, cierto si todas las filas de una matriz suman lo mismo
check_sum([],_).
check_sum([Fila|Filas], Sum) :-
    sumlist(Fila,Sum),
    check_sum(Filas,Sum).

% times/3, cierto si el tercer argumento es la multiplicacion de los dos primeros
times(X,0,0):- nat(X).
times(X,s(Y),Z):-
    times(X,Y,W),
    plus(X,W,Z).

% exp/3, cierto si tercer argumento es el segundo argumento elvado al primero
exp(0,X,s(0)):- nat(X).
exp(s(N),X,Y):-
    exp(N,X,W),
    times(W,X,Y).

% greater_zero/1, cierto si el número es mayor que 0.
greater_zero(s(0)).
greater_zero(s(N)) :-
    greater_zero(N).

% square_lists/3, cierto si SQ es una matriz cuadrada de N*N, cuyas filas suman S
square_lists(N,SQ,S) :-
    greater_zero(N),    % N tiene que ser mayor que 0
    exp(s(s(0)),N,N2),  % N2 será N al cuadrado
    nums(N2,Lista),     % Lista tendrá todos los elementos de 1 a N2
    perm(Lista,ListaP), % Se hacen las permutaciones necesarias
    make_matrix(ListaP,N,SQ), % Se forma la matriz de N filas
    check_sum(SQ,S). % Se comprueba que todas sumen lo mismo
