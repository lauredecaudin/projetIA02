:- module(bot,
[  get_moves/3
]).
:- dynamic choose4/3.
:- dynamic majBoard/3.
:- dynamic inserer/1.

% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ])
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[3,7,cat,silver],[4,7,horse,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

%exemple dappel moins long
%get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver]]).

% default call
%get_moves([[[1,0],[2,0]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).

% Faut lire le README du Git de P16, ya pleins de prédicats utiles dont on pourrait peut être sinspirer
%https://github.com/vincebhx/IA02-Khan/blob/master/README.md

%Git du projet
%https://github.com/rlacazel/Prolog-Arimaa

%predicat not
not(P) :- P, !, fail.
not(P).

strength(rabbit, 1).
strength(cat, 2).
strength(dog, 3).
strength(horse, 4).
strength(camel, 5).
strength(elephant, 6).


%predicat element dune liste
element(X,[X|_]):-!.
element(Y,[X|L]) :- element(Y,L).


%cas general et generique
aCote1([L,C,X,Y], [L2,C,U,V]) :-  piece(L, C, X,Y),L2 is L+1, L2<8, piece(L2, C, U,V).
aCote1([L,C,X,Y], [L2,C,U,V]) :-  piece(L, C, X,Y),L2 is L-1, L2>=0, piece(L2, C, U,V).
aCote1([L,C,X,Y], [L,C2,U,V]) :-  piece(L,C, X,Y),C2 is C-1, C2>=0, piece(L, C2, U,V).
aCote1([L,C,X,Y], [L,C2,U,V]) :-  piece(L,C,X,Y),C2 is C+1, C2<8, piece(L, C2,U,V).
aCote2([L,C,X,Y], [L2,C2,U,V]) :-  piece(L,C,X,Y),L2 is L+1, C2 is C+1, L2<8, C2<8, piece(L2, C2,U,V).
aCote2([L,C,X,Y], [L2,C2,U,V]) :-  piece(L,C,X,Y), L2 is L-1, C2 is C+1, L2>=0, C2<8, piece(L2, C2,U,V).
aCote2([L,C,X,Y], [L2,C2,U,V]) :-  piece(L, C, X,Y), L2 is L-1, C2 is C-1, L2>=0, C2>=0,piece(L2, C2, U,V).
aCote2([L,C,X,Y], [L2,C2,U,V]) :-  piece(L, C, X,Y),L2 is L+1, C2 is C-1, L2<8, C2>=0, piece(L2, C2, U,V).

%aCote2 correspond aux diago (on separe car pour push et pull on ne peut pas push ou pull une piece en diago)

%aCotebis prend en compte les diagonales
aCotebis([L1,C1,X,Y],[L2,C2,U,V]) :- aCote1([L1,C1,X,Y],[L2,C2,U,V]).
aCotebis([L1,C1,X,Y],[L2,C2,U,V]) :- aCote1([L2,C2,U,V],[L1,C1,X,Y]).
aCotebis([L1,C1,X,Y],[L2,C2,U,V]) :- aCote2([L1,C1,X,Y],[L2,C2,U,V]).
aCotebis([L1,C1,X,Y],[L2,C2,U,V]) :- aCote2([L2,C2,U,V],[L1,C1,X,Y]).

%frozen
frozen(L,C,X,W) :- aCotebis([L,C,X,W],[L2,C2,Y,Z]), strength(X,N), strength(Y,M), N<M, W \= Z, \+aCotebis([L,C,X,W],[L3,C3,A,W]).

%ajout au tableau des frozen
frozenTab([[L,C,X,Y]|L]) :- piece(L,C,X,Y), frozen(L,C,X,Y), frozenTab(L), \+element([L,C,X,Y], L).

%predicat trap
trap(X,Y) :- piece(2,2,X,Y).
trap(X,Y) :- piece(5,2,X,Y).
trap(X,Y) :- piece(2,5,X,Y).
trap(X,Y) :- piece(5,5,X,Y).

%ajout au tableau des capturés
captured([[X,Y]|L]) :- trap(X,Y), captured(L).

% Predicat gamestate
gamestate(X, Y, Z, U) :-  U=<4 ,captured(Y), frozenTab(Z), remainSteps(U).

concat([],L,L).
concat([X|L1], L2,[X|L3]) :- concat(L1,L2,L3).

%prédicat Free (place libre)
free(L,C) :- \+ piece(L,C,_,_).

%supp toutes les occurences
suppr([],X,[]).
suppr([X|Ist],X,SansX) :- suppr(Ist,X,SansX).
suppr([X|Ist],Z,[X|SansX]) :- Z \= X , suppr(Ist,Z,X).

%sens du mouvement demandé
sens(gauche).
sens(droite).
sens(bas).
sens(haut).

%predicat possMove, en supposant silver en haut et gold en bas
%on peut pas bouger les out ou frozen
%cas special des lapins qui peuvent pas aller backward

%lapins

possMove(L,C,rabbit,silver,[[L,C],[L,C2]]) :-  piece(L,C,rabbit,silver), \+frozen(L,C,rabbit, silver), C2 is C+1, C2>=0, C2<8,\+piece(L,C2,_,_).
possMove(L,C,rabbit,silver,[[L,C],[L,C2]]) :-  piece(L,C,rabbit,silver), \+frozen(L,C,rabbit, silver),C2 is C-1, C2>=0, C2<8,\+piece(L,C2,_,_).
possMove(L,C,rabbit,silver,[[L,C],[L2,C]]) :-  piece(L,C,rabbit,silver),\+frozen(L,C,rabbit, silver), L2 is L+1, L2<8, \+piece(L2,C,_,_).

%general

possMove(L,C,X,silver,[[L,C],[L2,C]]) :- piece(L,C,X,silver), \+frozen(L,C,X,silver), L2 is L-1, L2>=0, L2<8, \+piece(L2,C,_,_).
possMove(L,C,X,silver,[[L,C],[L,C2]]) :-  piece(L,C,X,silver), \+frozen(L,C,X,silver),C2 is C+1, C2>=0, C2<8, \+piece(L,C2,_,_).
possMove(L,C,X,silver,[[L,C],[L,C2]]) :-  piece(L,C,X,silver), \+frozen(L,C,X,silver),C2 is C-1, C2>=0, C2<8, \+piece(L,C2,_,_).
possMove(L,C,X,silver,[[L,C],[L2,C]]) :-  piece(L,C,X,silver), \+frozen(L,C,X,silver),L2 is L+1, L2>=0, L2<8, \+piece(L2,C,_,_).


% X : pièce poussant(son type), W : pièce poussée(son type), N : nombre de coup restant, (L, C) :position de la piece à pousser
%choix du sens
%possPush(X,silver,W,L,C) :- !, (L+1)=<7,  free(L2,C), L2 is L+1,sens(bas), piece(L, C, W,gold),aCote(X,W), strength(W,S1), strength(X,S2), S1<S2.
%possPush(X,silver,W,L,C) :-!,  (C+1)=<7,  free(L,C2), C2 is C+1,sens(droite), piece(L, C, W, gold),aCote(X,W), strength(W,S1), strength(X,S2), S1<S2.
%possPush(X,silver,W,L,C) :-!, (C-1)>=0,  free(L,C2), C2 is C-1,sens(gauche),  piece(L, C, W, gold),aCote(X,W),strength(W,S1), strength(X,S2), S1<S2.
%possPush(X,silver,W,L,C) :- !, (L-1)>=0,  free(L2,C), L2 is L-1,sens(haut),  piece(L, C, W, gold),aCote(X,W),strength(W,S1), strength(X,S2), S1<S2.

%Retourne lensemble des mouvements par pull possibles
%possPull(X,silver,W,L,C) :- piece(L, C, W,gold),piece(L2,C,X,silver),free(L3,C), L2 is L-1, L3 is L-2, L-2>=0,strength(W,S1), strength(X,S2), S1<S2.
%possPull(X,silver,W,L,C) :- piece(L, C, W,gold),piece(L2,C,X,silver),free(L3,C), L2 is L+1, L3 is L+2, L+2=<7,strength(W,S1), strength(X,S2), S1<S2.
%possPull(X,silver,W,L,C) :- piece(L, C, W,gold),piece(L,C2,X,silver),free(L,C3), C2 is C+1, C3 is C+2, C+2=<7,strength(W,S1), strength(X,S2), S1<S2.
%possPull(X,silver,W,L,C) :- piece(L, C, W,gold),piece(L, C2, X,silver),free(L,C3), C2 is C-1, C3 is C-2, C-2>=0,strength(W,S1), strength(X,S2), S1<S2.

%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- L2 is L1-1, possPull(_,_,_,L,C), L=:=L1+1, possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :-L2 is L1+1, possPull(_,_,_,L,C), L=:=L1-1, possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]):- C2 is C1-1, possPull(_,_,_,L,C), C=:=C1+1, possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- C2 is C1+1, possPull(_,_,_,L,C), C=:=C1-1, possmove(_,_,S),!.

%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- L2 is L1+1, possPush(_,_,_,L,C), L=:=L1+1, possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- L2 is L1-1, possPush(_,_,_,L,C), L=:=L1-1, possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- C2 is C1+1, possPush(_,_,_,L,C), C=:=C1+1,possmove(_,_,S),!.
%possmove(_,_,[[[L1,C1],[L2,C2],2]|S]) :- C2 is C1-1, possPush(_,_,_,L,C), C=:=C1-1, possmove(_,_,S), !.


%predicat formation base de données
inserer([]) :- !.
inserer([[A,B,C,D]|L]):- asserta(piece(A,B,C,D)), inserer(L).

%predicat ajouter dans une liste
add(E,L,[E|L]).

%predicat retirer dune liste
retirer(_,[],_) :- !.
retirer(E,[E|Q],Q) :- !.
retirer(E,[T|Q],[T|R]):- retirer(E,Q,R).

%predicat remplacer element 1 par elem2 dans une liste
remplacer(A,B,L,L2) :- add(A,L,L1), retirer(B,L1,L2).

sans_doublon([],[]).
sans_doublon([X|L],R):- member(X,L),sans_doublon(L,R).
sans_doublon([X|L],[X|R]):- \+member(X,L),sans_doublon(L,R).

majBoard([],_,_).
majBoard([[L1,C1],[L2,C2]],Board,NewBoard) :- remplacer([L2,C2,X,Y],[L1,C1,X,Y],Board,NewBoard), retract(piece(L1,C1,X,Y)), asserta(piece(L2,C2,X,Y)),!.

moves([],[_]):- !.
moves(M, Board) :-  findall(Res,possMove(_,_,_,silver,Res),M).

%       STRATEGIES
%si lapin en avant derniere ligne
choose([[6,C],[7,C]], Board) :- moves(S, Board), element([[6,C],[7,C]], S), piece(6,C,rabbit, silver).

%si piece adverse quon peut frozen (dans les 3 lignes les plus proches de nous)
%par le haut
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S), L2=<3, L2=:=L1+1, L3 is L2+1,piece(L3,C1,rabbit, gold), \+aCotebis([L3,C1,rabbit,gold],[_,_,_,gold]).
%par le bas
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S), L2=:=L1-1, L2=<3, L3 is L2-1, piece(L3,C1,rabbit, gold), \+aCotebis([L3,C1,rabbit,gold],[_,_,_,gold]).
%par la gauche
choose([[L1,C1],[L1,C2]], Board) :- moves(S, Board), element([[L1,C1],[L1,C2]], S), C2=:=C1+1,L1=<3, C3 is C2+1, piece(L1,C3,rabbit, gold), \+aCotebis([L1,C3,rabbit,gold],[_,_,_,gold]).
%par la droite
choose([[L1,C1],[L1,C2]], Board) :- moves(S, Board), element([[L1,C1],[L1,C2]], S), C2=:=C1-1, L1=<3, C3 is C2-1, piece(L1,C3,rabbit, gold), \+aCotebis([L1,C3,rabbit,gold],[_,_,_,gold]).

%mettre piece amie pres dun lapin avance
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S), L2=:=L1+1, L2>3, L3 is L2+1,piece(L3,C1,rabbit, silver),piece(L1,C1,_,silver).
choose([[L1,C1],[L1,C2]], Board) :- moves(S, Board), element([[L1,C1],[L1,C2]], S), C2=:=C1+1, L1>4, C3 is C2+1,piece(L1,C3,rabbit, silver),piece(L1,C1,_,silver).
choose([[L1,C1],[L1,C2]], Board) :- moves(S, Board), element([[L1,C1],[L1,C2]], S), C2=:=C1-1, L1>4, C3 is C2+1,piece(L1,C3,rabbit, silver),piece(L1,C1,_,silver).

%avancer lapin mais pas si il risque detre frozen
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S),piece(L1,C1,rabbit, silver), L2=:=L1+1, L3 is L2+1, piece(L3,C1,_,silver).
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S),piece(L1,C1,rabbit, silver), L2=:=L1+1, L3 is L2+1, \+piece(L3,C1,T,gold), T\=rabbit.


%avancer un elephant vers les traps du côté adverse
%on est proche dun trap donc on se met a cote
choose([[4,1],[5,1]], Board) :- moves(S, Board), element([[4,1],[5,1]], S),piece(4,1,elephant, silver).
choose([[4,3],[5,3]], Board) :- moves(S, Board), element([[4,3],[5,3]], S),piece(4,3,elephant, silver).
choose([[4,4],[5,4]], Board) :- moves(S, Board), element([[4,4],[5,4]], S),piece(4,4,elephant, silver).
choose([[4,6],[5,6]], Board) :- moves(S, Board), element([[4,6],[5,6]], S),piece(4,6,elephant, silver).

%on avance lelephant si il est sur une des colonnes pratiques (1,2,3,4,5,6) pour atteindre les traps, mais on ne lavance plus une fois quil est a cote dun trap
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S), C1\=1, C1\=7, L1<5, piece(L1,C1,elephant, silver), L2=:=L1+1.


%choisir mouvement quelconque
choose([[L1,C1],[L2,C1]], Board) :- moves(S, Board), element([[L1,C1],[L2,C1]], S), L2=:=L1+1, piece(L1,C1,A, silver), L3 is L2+1, \+piece(L3,C1,T,gold), strength(A,S1), strength(T,S2), S1<S2.

choosefirst(Mvmt,Board,NewBoard):- findall(Res, choose(Res,Board),[Mvmt|L]), majBoard(Mvmt,Board,NewBoard),!.

choose4(A,_,4).
choose4([A|M],Board,N) :-  retractall(piece(_)), inserer(Board), N1 is N+1, choosefirst(A,Board,Newboard), choose4(M,Newboard,N1).

get_moves([A,B,C,D], Gamestate, Board) :-  choose4([A,B,C,D|_],Board,0).

consult(arimaa.pl).
