:- module(_,_,[assertions]).
:- use_package(nativeprops).
:- use_module(library(unittest/unittest_props)).

alumno_prode('Lopez','Garcia','Guillermo','a180182').

:- doc(title, "Sumas de pares de listas y cuadrados").
:- doc(author, "Guillermo Lopez Garcia, a180182").

:- doc(module, "Este modulo define dos programas, sumlists/4 y square_lists/3.

Para el primero, dado un numero N par, se devuelven dos listas L1 y L2 que contienen 
entre las dos los números de Peano de 1 a N y cuya suma es la misma, S.
@subsection{Ejemplos de sumlists/4:}
@begin{enumerate}
@item 1)
@begin{verbatim}
?- sumlists(s(s(s(s(0)))),L1,L2,S).

L1 = [s(s(s(0))),s(s(0))],
L2 = [s(s(s(s(0)))),s(0)],
S = s(s(s(s(s(0))))) ? 
yes
?-
@end{verbatim}
@item
@begin{verbatim}
?- sumlists(s(s(s(s(s(s(s(s(0)))))))),L1,L2,S).

L1 = [s(s(s(s(s(s(s(0))))))),s(s(s(s(s(0))))),s(s(s(s(0)))),s(s(0))],
L2 = [s(s(s(s(s(s(0)))))),s(s(s(s(s(s(s(s(0)))))))),s(s(s(0))),s(0)],
S = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))))))))) ? 
yes
?-
@end{verbatim}
@end{enumerate}

Por otra parte, para square_lists/4, dado un numero N, se devuelve una matriz cuadrada
de N*N que contiene todos los numeros de Peano del 1 a N^2 y cuyas filas suman lo mismo.
@subsection{Ejemplos de square_lists/3:}
@begin{enumerate}
@item
@begin{verbatim}
?- square_lists(s(s(0)),SQ,S).

S = s(s(s(s(s(0))))),
SQ = [[s(s(s(0))),s(s(0))],[s(s(s(s(0)))),s(0)]] ? 

yes
?- 
@end{verbatim}
@item 2)
@begin{verbatim}
?- square_lists(s(s(s(0))),SQ,S).

S = s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))),
SQ = [[s(s(s(s(s(s(s(s(0)))))))),s(s(s(s(0)))),s(s(s(0)))],
     [s(s(s(s(s(s(s(0))))))),s(s(s(s(s(s(0)))))),s(s(0))],
     [s(s(s(s(s(s(s(s(s(0))))))))),s(s(s(s(s(0))))),s(0)]] ? 

yes
?-
@end{verbatim}
@end{enumerate}

Mas adelante se muestran los predicados y propiedades que se han utilizado.
").

% PARTE 1


:- prop nat(N) #"Cierto si @var{N} es un numero natural. @includedef{nat/1}".

nat(0).
nat(s(X)) :- nat(X).


:- prop lista(L) #"Cierto si @var{L} es una lista. @includedef{lista/1}".

lista([]).
lista([_|Y]):-
    lista(Y).


:- pred plus(A,B,C) #"Cierto si @var{A} + @var{B} = @var{C}. @includedef{plus/3}".

plus(X,0,X) :-
    nat(X).
plus(X,s(Y),s(Z)) :-
    plus(X,Y,Z).


:- pred nums(N,L) #"Cierto si @var{L} es una lista descendente de @var{N} a 1. @includedef{nums/2}".

nums(0,[]).
nums(s(N),[s(N)|Np]) :-
    nums(N,Np).


:- pred sumlist(L,N) #"Cierto si @var{N} es la suma de elementos de @var{L}. @includedef{sumlist/2}".

sumlist([],0).
sumlist([N|Np],S) :-
    sumlist(Np,Sp),  % se llega recursivamente al caso base
    plus(N,Sp,S).    % se suman los valores recursivamente


:- pred choose_one(E,L,R) #"Cierto si @var{R} es igual a @var{L} sin el elemento @var{E}. @includedef{choose_one/3}".

choose_one(E,[E|Lp],Lp) :- % caso base, si se ha encontrado E y el resto es lo mismo
    lista(Lp).
choose_one(E,[X|Lp],[X|Rp]) :-
    choose_one(E,Lp,Rp).


:- pred perm(L,Lp) #"Cierto si @var{Lp} es una permutacion de @var{L}. @includedef{perm/2}".

perm([],[]). % caso base, dos listas vacías
perm([X|R],L) :-
    perm(R,Lp), % de este modo llega al último elemento recursivamente
    choose_one(X,L,Lp).  % con las repeticiones se va creando L


:- pred split(L,Lp,Li) #"Cierto si @var{Lp} tiene los elementos de posicion par de @var{L}, y @var{Li}, los de posicion impar. @includedef{split/3}".

split([],[],[]). %caso base, las tres listas están vacías
split([X1,X2|Xn],[X1|Xp],[X2|Xpp]) :- % el primer argumento asegura que tenga un nº par de elementos
    split(Xn,Xp,Xpp).


:- pred sumlists(N,L1,L2,S) #"Cierto si @var{L1} y @var{L2} contienen entre las dos los naturales de @var{N} hasta 1, y ambas suman lo mismo. 
Los ejemplos de uso se encuentran al principio del documento.@includedef{sumlists/4}".

sumlists(N,L1,L2,S) :-
    nums(N,L),        % L es una lista desde N a 1
    perm(L,Lp),       % Lp tiene que ser una permutación de L
    split(Lp,L1,L2),  % Lp es una composición de L1 y L2
    sumlist(L1,S),    % S es el resultado de la suma de elementos de L1 y L2
    sumlist(L2,S).



% PARTE 2


:- pred make_matrix(L,N,M)
   #"Cierto si @var{M} es una matriz de @var{N} elementos por fila, formada por los elementos de @var{L}. @includedef{make_matrix/3}".

make_matrix([], _, []).
make_matrix(Lista, N, [Fila|Filas]):-
  take_N(Lista, N, Fila, Rest),  % se cogen los N elementos de Lista y se guardan en Fila
  make_matrix(Rest, N, Filas).   % se hace lo mismo con el resto de la lista


:- pred take_N(L1,N,L2,Resto)
   #"Cierto si @var{L2} es una lista formada por los primeros @var{N} elementos de @var{L1}. @var{Resto} contiene el resto de elementos de @var{L1}. @includedef{take_N/4}".

take_N(Rest, 0, [], Rest).
take_N([Elem|Lista], s(N), [Elem|Lista2], Rest):-
  take_N(Lista, N, Lista2, Rest).


:- pred check_sum(M,S) #"Cierto si la suma de todas las filas de @var{M} suman @var{S}. @includedef{check_sum/2}".

check_sum([],_).
check_sum([Fila|Filas], Sum) :-
    sumlist(Fila,Sum),
    check_sum(Filas,Sum).


:- pred times(A,B,C) #"Cierto si @var{A} * @var{B} = @var{C} @includedef{times/3}".

times(X,0,0):- nat(X).
times(X,s(Y),Z):-
    times(X,Y,W),
    plus(X,W,Z).


:- pred exp(Exp,N,S) #"Cierto si @var{N}^@var{Exp} = @var{S} @includedef{exp/3}".

exp(0,X,s(0)):- nat(X).
exp(s(N),X,Y):-
    exp(N,X,W),  % la primera vez va al caso base
    times(W,X,Y).% se va multiplicando las veces que indique el exponente


:- prop greater_zero(N) #"Cierto si @var{N} es un natural mayor que 0. @includedef{greater_zero/1}".

greater_zero(s(0)).
greater_zero(s(N)) :-
    greater_zero(N).


:- pred square_lists(N,SQ,S) #"Cierto si @var{SQ} es una matriz de @var{N}*@var{N}, cuyas filas suman @var{S}, y entre todas contienen los numeros de @var{N}^2 hasta 1. Los tests se encuentran al principio del documento. @includedef{square_lists/3}".

square_lists(N,SQ,S) :-
    greater_zero(N),    % N tiene que ser mayor que 0
    exp(s(s(0)),N,N2),  % N2 será N al cuadrado
    nums(N2,Lista),     % Lista tendrá todos los elementos de 1 a N2
    perm(Lista,ListaP), % Se hacen las permutaciones necesarias
    make_matrix(ListaP,N,SQ), % Se forma la matriz de N filas
    check_sum(SQ,S). % Se comprueba que todas sumen lo mismo



% TESTS
:- test plus(A,B,C) : (A = s(s(0)), B = s(0)) => (C = s(s(s(0)))) + not_fails # "Caso: 2 + 1 = 3".
:- test plus(A,B,C) : (A = 0, B = s(0)) => (C = s(0)) + not_fails # "Caso: 0 + 1 = 1".
:- test plus(A,B,C) : (A = s(s(0)), C = s(s(s(s(0))))) => (B = s(s(0))) + not_fails # "Ejemplo de una resta: 4 - 2 = 2".

:- test nums(N,L) :
   (N=s(s(s(0)))) => (L=[s(s(s(0))),s(s(0)),s(0)]) + not_fails #"Lista desde el 3 hasta  a 1.".
:- test nums(N,L) :
   (L=[s(s(s(0))),s(s(0)),s(0)]) => (N=s(s(s(0)))) + not_fails #"Igual que el caso anterior pero nos dan la lista.".
:- test nums(N,L) :
   (N=0) => (L=[]) + not_fails #"Caso base.".

:- test sumlist(L,N) : (L=[s(0),s(0),s(s(0))]) => (N=s(s(s(s(0))))) + not_fails.
:- test sumlist(L,N) : (L=[0,0,0]) => (N=0) + not_fails.
:- test sumlist(L,N) : (L=[]) => (N=0) + not_fails #"Caso base.".

:- test choose_one(E,L,R) :
   (E=s(0),L=[s(s(0)),s(s(s(0))),s(0)]) => (R=[s(s(0)),s(s(s(0)))]) + not_fails #"Se quita un elemento de la lista.".
:- test choose_one(E,L,R) :
   (E=s(s(s(0))),L=[s(s(0)),s(s(s(0))),s(0)]) => (R=[s(s(0)),s(0)]) + not_fails #"Se quita un elemento de la lista.".
:- test choose_one(E,L,R) :
   (E=s(s(0)),L=[s(s(0))]) => (R=[]) + not_fails #"Se quita un elemento de una lista de un elemento.".

:- test perm(L,Lp) : (L = []) => (Lp = []) + not_fails #"Caso base.".
% En la documentacion, el siguiente test no se genera bien.
:- test perm(L,Lp) :
   ( L = [a,b,c] )
    + (
            try_sols(6),
            solutions([
                         perm([a,b,c],[a,b,c]),
                         perm([a,b,c],[b,a,c]),
                         perm([a,b,c],[b,c,a]),
                         perm([a,b,c],[a,c,b]),
                         perm([a,b,c],[c,a,b]),
                         perm([a,b,c],[c,b,a])
                  ])
    ) #"Permutacion de una lista de 3 elementos. Al usar solutions en el test, LPdoc no genera bien la documentacion para este caso. En el codigo se muestra el test original.".


:- test split(L,L1,L2) : (L = []) => (L1 =[],L2 = []) + not_fails #"Caso base.".
:- test split(L,L1,L2) : (L = [a,b,c,d]) => (L1 =[a,c],L2 = [b,d]) + not_fails #"Caso normal dada una lista L.".
:- test split(L,L1,L2) : (L1 =[a,c],L2 = [b,d]) => (L = [a,b,c,d]) + not_fails #"Dadas las dos listas devuelve la original.".

:- test make_matrix(L,N,M)
   : (L = [], N=s(0)) => (M=[]) + not_fails #"Caso base.".
:- test make_matrix(L,N,M)
   : (L = [a,b,c,d], N=s(s(0))) => (M=[[a,b], [c,d]]) + not_fails #"Caso con una lista de 4 elementos y filas de 2.".
:- test make_matrix(L,N,M) :
   (L = [a,b,c,d,e,f,g,h], N=s(s(s(s(0))))) => (M =[[a,b,c,d], [e,f,g,h]]) + not_fails # "Caso con una lista de 8 elementos y filas de 4.".

:- test take_N(L1,N,L2,Resto) :
   (L1 = [a,b,c,d,e], N = s(s(0)), L2 = [a,b]) => ( Resto = [c,d,e]) + not_fails #"Se toman 2 elementos de una lista de 5.".
:- test take_N(L1,N,L2,Resto) :
   (L1 = [a,b,c,d,e], N = 0) => (L2 = [], Resto = [a,b,c,d,e]) + not_fails #"Se toman 0 elementos de una lista de 5.".

:- test check_sum(M,S) :
   ( M = [[s(s(0)),s(s(s(0)))], [s(0),s(s(s(s(0))))]] ) => (S = s(s(s(s(s(0)))))) + not_fails #"Matriz de 2*2 cuyos elementos suman 5.".
:- test check_sum(M,S) :
   ( M = [[s(s(0))], [s(s(0))],[s(s(0))]] ) => (S = s(s(0))) + not_fails #"Matriz de 3*1 cuyos elementos suman 2.".

:- test times(A,B,C) :
   (A = s(s(0)), B = 0) => (S = 0) + not_fails #"Caso base: 2 * 0 = 0".
:- test times(A,B,C) :
   (A = s(s(0)), B = s(s(s(0)))) => (S = s(s(s(s(s(s(0))))))) + not_fails #"Caso: 2 * 3 = 6".

:- test exp(Exp,N,S) :
   (Exp = 0, N=s(s(0))) => (S = s(0)) + not_fails #"Caso base: 2^0 = 1".
:- test exp(Exp,N,S) :
   (Exp = s(s(s(0))), N=s(s(0))) => (S = s(s(s(s(s(s(s(s(0))))))))) + not_fails #"Caso: 2^3 = 8".




                        
                                                




