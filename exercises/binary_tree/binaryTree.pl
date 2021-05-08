:- module(_,_,[assertions]).


list([]).
list([_X|Y]) :-
    list(Y).

append([],Ys ,Ys) :-
    list(Ys).
append([X|Xs],Ys ,[X|Zs]) :-
    append(Xs ,Ys ,Zs).

%definition of a binary tree
binary_tree(void).
binary_tree(tree(_Element,Left,Right)) :-
    binary_tree(Left),
    binary_tree(Right).

%true if X is a member of the tree
tree_member(X,tree(X,Left,Right)) :-
    binary_tree(Left),
    binary_tree(Right).
tree_member(X,tree(_Y,Left,_Right)) :-
    tree_member(X,Left).
tree_member(X,tree(_Y,_Left,Right)) :-
    tree_member(X,Right).

%pre_order list of the elements
pre_order(void ,[]).
pre_order(tree(X,Left,Right),Elements) :-
    pre_order(Left,ElementsLeft),
    pre_order(Right,ElementsRight),
    append([X|ElementsLeft],ElementsRight,Elements).

%in_order list of the elements
in_order(void,[]).
in_order(tree(X,L,R),E) :-
    in_order(L,RL), 
    in_order(R,RR),
    append(RL,[X|RR],E).

%post_order list of the elements
post_order(void,[]).
post_order(tree(Element,Left,Right),Elements) :-
    post_order(Left,ElementsLeft),
    post_order(Right,ElementsRight),
    append(ElementsLeft,ElementsRight,E),
    append(E,[Element],Elements).
