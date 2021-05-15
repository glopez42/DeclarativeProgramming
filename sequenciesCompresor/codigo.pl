:- module(_,_,[classic,assertions,regtypes]).
:- use_package(nativeprops).
:- use_module(library(unittest/unittest_props)).

alumno_prode('Lopez','Garcia','Guillermo','a180182').

:- doc(title, "Compresion de secuencias").
:- doc(author, "Guillermo Lopez Garcia, a180182").

:- doc(module, "Este modulo define el programa comprimir/2, que dada una lista de caracteres devuelve la compresion adecuada, siguiendo las reglas del enunciado de la practica.

@subsection{Ejemplos:}
@begin{enumerate}
@item
@begin{verbatim}
?- comprimir([a,b,b,b,a,b,b,b],R).

R = ['(',a,b,3,')',2] ? 

yes
?-
@end{verbatim}
@item
@begin{verbatim}
?- comprimir([a,b,a,b,a,b,c],R).

R = ['(',a,b,')',3,c] ? 

yes
?- 
@end{verbatim}
@item
@begin{verbatim}
?- comprimir([a,a,a,a,a,a,a,a,a],R).

R = [a,9] ? 

yes
?- 
@end{verbatim}

@end{enumerate}

").

:- pred memo(Inicial,Comprimida) #"Predicado dinamico para guardar compresiones.".
:- dynamic memo/2.

:- pred comprimir(Inicial,Comprimida)
   #"Cierto si la secuencia @var{Comprimida} es la compresion de @var{Inicial}.@includedef{comprimir/2}". 
comprimir(Inicial,Comprimida) :-
    limpia_memo,
    compresion_recursiva(Inicial,Comprimida).

:- pred limpia_memo
   #"Predicado que limpia la memoria dinamica antes de empezar la compresion recursiva.@includedef{limpia_memo/0}".
limpia_memo :-
    retractall(memo(_,_)).

:- pred compresion_recursiva(In,Com)
   #"Predicado para comprimir recursivamente una secuencia original.@includedef{compresion_recursiva/2}".
compresion_recursiva(Inicial,Comprimida) :-
    mejor_compresion_memo(Inicial,Comprimida).

:- pred partir(Todo,P1,P2)
   #"Cierto si @var{P1} y @var{P2} no son listas vacias y concatenadas forman @var{Todo}.@includedef{partir/3}".
partir(Todo, Parte1, Parte2) :-
    append(Parte1,Parte2,Todo),
    notEmptyList(Parte1),
    notEmptyList(Parte2).

:- pred parentesis(Parte,Num,ParteNum)
   #"Predicado que añade parentesis a la secuencia @var{Parte}, antes de @var{Num}. Se añaden solo si la secuencia tiene una longitud >= 2. @includedef{parentesis/3}".
parentesis(Parte,Num,ParteNum) :-
    number(Num), %se comprueba que sea un numero
    length(Parte,L),
    %si la lista tiene mas de dos elementos se ponen los parentesis
    ( L >= 2 ->
        partir(Lista1,['('],Parte),
        partir(Lista2,Lista1,[')']),
        partir(ParteNum,Lista2,[Num])
    ;
        partir(ParteNum,Parte,[Num])
    ).

:- pred se_repite(Cs,Parte,Num0,Num)
   #"Cierto si @var{Cs} se obtiene por repeticiones de la secuencia @var{Parte}. @includedef{se_repite/4}" .
se_repite(Cs,Parte,Num0,Num) :-
    n_repeticiones(Cs,Parte,N),
    Num is Num0 + N. %se guarda en Num el numero total de repeticiones


%FASE A

:- pred repeticion(Todo,Resultado) #"Predicado que identifica una parte que repetida forme la secuencia @var{Todo}.@includedef{repeticion/2}".
repeticion(Todo,Resultado):-
    %de esta forma prueba con todas las partes posibles
    partir(Todo,Parte1,_Parte2),
    se_repite(Todo,Parte1,0,N),
    compresion_recursiva(Parte1,Comprimida),
    parentesis(Comprimida,N,Resultado).

%FASE B

:- pred compresion(Inicial,Comprimida)
   #"Predicado que realiza una compresion de @var{Inicial} por repeticion o division. @includedef{compresion/2}".
compresion(Inicial,Comprimida) :-
    repeticion(Inicial,Comprimida).
compresion(Inicial,Comprimida) :-
    division(Inicial,Comprimida).

:- pred division(Todo,Resultado)
   #"Predicado que divide una secuencia en dos e intenta comprimirlas recursivamente. @includedef{division/2}".
division(Todo,Resultado):-
    partir(Todo,Parte1,Parte2),
    compresion_recursiva(Parte1,Comprimida1),
    compresion_recursiva(Parte2,Comprimida2),
    append(Comprimida1,Comprimida2,Resultado).


%FASE C

:- pred mejor_compresion(Inicial,Compresion)
   #"Predicado que busca la mejor compresion de la secuencia @var{Inicial}. @includedef{mejor_compresion/2}". 
mejor_compresion(Inicial,Comprimida) :-
    findall(X,compresion(Inicial,X),[Lista1|Resto]),
    length(Inicial,LongMax),
    %coge la lista de menor longitud
    shortest_list([Lista1|Resto],Lista1,ResultadoTemporal,LongMax),
    %si la mejor solucion es >= que la que original, se deja la original
    length(ResultadoTemporal,LTemp),
    ( LTemp >= LongMax ->
        Comprimida = Inicial
    ;
        Comprimida = ResultadoTemporal
    ),
    !. %No se necesita que siga buscando+

mejor_compresion(Inicial,Inicial). %Caso base


%FASE D

:- pred mejor_compresion_memo(Inicial,Comprimida)
   #"Predicado que llama a mejor_compresion/2 despues de comprobar si la secuencia ya se ha comprimido anteriormente, y por tanto, se encuentra en el predicado memo/2. @includedef{mejor_compresion_memo/2}".
mejor_compresion_memo(Inicial,Comprimida) :-
    memo(Inicial,Comprimida), %comprueba si la secuencia ya se ha comprimido anteriormente 
    !.
mejor_compresion_memo(Inicial,Comprimida) :-
    mejor_compresion(Inicial,Comprimida),
    assert(memo(Inicial,Comprimida)).


%Programas auxiliares

:- pred notEmptyList(Lista)
   #"Cierto si @var{Lista} es una lista no vacia. @includedef{notEmptyList/1}".
notEmptyList([_Elem]).
notEmptyList([_Elem|Rest]):-
    list(Rest),
    notEmptyList(Rest).

:- pred n_repeticiones(Todo,Parte,N)
   #"Cierto si @var{N} es el numero de repeticiones de @var{Parte}, en @var{Todo}.@includedef{n_repeticiones/3}".
n_repeticiones([],_Parte,0).
n_repeticiones(Todo,Parte,N) :-
    append(ListaAux,Parte,Todo),
    n_repeticiones(ListaAux,Parte,Np),
    N is Np + 1.

shortest_list([],Resultado,Resultado,_N).
shortest_list([Lista|Resto],Shortest,Resultado,N) :-
    length(Lista,Length),
    N =< Length,
    shortest_list(Resto,Shortest,Resultado,N).
shortest_list([Lista|Resto],_Shortest,Resultado,N) :-
    length(Lista,Length),
    N > Length,
    shortest_list(Resto,Lista,Resultado,Length).
