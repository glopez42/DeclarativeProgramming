:- module(_,_,[classic,assertions,regtypes]).

alumno_prode('Lopez','Garcia','Guillermo','a180182').


comprimir(Inicial,Comprimida) :-
    limpia_memo,
    compresion_recursiva(Inicial,Comprimida).
limpia_memo.
compresion_recursiva(Inicial ,Inicial).


partir(Todo, Parte1, Parte2) :-
    append(Parte1,Parte2,Todo),
    notEmptyList(Parte1),
    notEmptyList(Parte2).

parentesis(Parte,Num,ParteNum) :-
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
