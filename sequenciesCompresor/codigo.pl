:- module(_,_,[classic,assertions,regtypes]).

alumno_prode('Lopez','Garcia','Guillermo','a180182').

:- dynamic memo/2.

comprimir(Inicial,Comprimida) :-
    limpia_memo,
    compresion_recursiva(Inicial,Comprimida).

limpia_memo :-
    retractall(memo(_,_)).

compresion_recursiva(Inicial,Comprimida) :-
    mejor_compresion_memo(Inicial,Comprimida).


partir(Todo, Parte1, Parte2) :-
    append(Parte1,Parte2,Todo),
    notEmptyList(Parte1),
    notEmptyList(Parte2).

parentesis(Parte,Num,ParteNum) :-
    number(Num),
    length(Parte,L),
    %si la lista tiene mas de dos elementos se ponen los parentesis
    ( L >= 2 ->
        append(['('],Parte,Lista1),
        append(Lista1,[')'],Lista2),
        append(Lista2,[Num],ParteNum)
    ;
        append(Parte,[Num],ParteNum)
    ).

se_repite(Cs,Parte,Num0,Num) :-
    n_repeticiones(Cs,Parte,N),
    Num is Num0 + N.


%FASE A

repeticion(Todo,Resultado):-
    %de esta forma prueba con todas las partes posibles
    partir(Todo,Parte1,_Parte2),
    se_repite(Todo,Parte1,0,N),
    compresion_recursiva(Parte1,Comprimida),
    parentesis(Comprimida,N,Resultado).


%FASE B

compresion(Inicial,Comprimida) :-
    repeticion(Inicial,Comprimida).
compresion(Inicial,Comprimida) :-
    division(Inicial,Comprimida).

division(Todo,Resultado):-
    partir(Todo,Parte1,Parte2),
    compresion_recursiva(Parte1,Comprimida1),
    compresion_recursiva(Parte2,Comprimida2),
    append(Comprimida1,Comprimida2,Resultado).


%FASE C

mejor_compresion(Inicial,Comprimida) :-
    findall(X,compresion(Inicial,X),[Lista1|Resto]),
    length(Inicial,LongMax),
    shortest_list([Lista1|Resto],Lista1,ResultadoTemporal,LongMax),
    length(ResultadoTemporal,LTemp),
    ( LTemp >= LongMax ->
        Comprimida = Inicial
    ;
        Comprimida = ResultadoTemporal
    ),
    !. %No se necesita que siga buscando
mejor_compresion(Inicial,Inicial).    


%FASE D

mejor_compresion_memo(Inicial,Comprimida) :-
    memo(Inicial,Comprimida),
    !.
mejor_compresion_memo(Inicial,Comprimida) :-
    mejor_compresion(Inicial,Comprimida),
    assert(memo(Inicial,Comprimida)).


%Programas auxiliares

%Cierto si se le pasa como argumento una lista no vacia
notEmptyList([_Elem]).
notEmptyList([_Elem|Rest]):-
    list(Rest),
    notEmptyList(Rest).

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
