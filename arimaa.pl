:- module(bot,
      [  get_moves/3
      ]).
	
% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ]) 
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
%get_moves([[[1,0],[2,0]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).

% Faut lire le README du Git de P16, ya pleins de prédicats utiles dont on pourrait peut être sinspirer*/ 
%https://github.com/vincebhx/IA02-Khan/blob/master/README.md

%Git du projet*/
%https://github.com/rlacazel/Prolog-Arimaa  */


%predicat not
not(P) :- P, !, fail.
not(P).

% predicat inferiorite
inf(X,Y) :- inf1(X,Y).
inf(X,Y) :- inf1(X,Z), inf(Z,Y).
inf1(rabbit, cat).
inf1(cat, dog).
inf1(dog, horse).
inf1(horse, camel).
inf1(camel, elephant).


%non en fait a part attribuer strength(cat, 2). strength(dog, 3). etc je vois pas
strength(rabbit, 1).
strength(cat, 2).
strength(dog, 3).
strength(horse, 4).
strength(camel, 5).
strength(elephant, 6).


%Petit prédicat pour trouver le joueur adverse*/
oppSide(silver, gold).
oppSide(gold, silver).

%predicat element liste
element(X,[X|_]).
element(Y,[X|L]) :- X \== Y, element(Y,L).


%Une pièce est définie par un tuple piece(type,side,Lin,Col,Etat), où :
%  Après en soit trength est déterminé par type donc devient inutile
% (Col, Lin) est la position de la pièce sur le plateau. (plateau de 8x8)
%  Etat détermine si la pièce est en jeu, si elle est en jeu et est , ou si elle est hors jeu (dans un piège); Etat peut prendre les valeurs 'in', 'frozen' ou 'out'.

%ici je trouve ça bizarre le position...
position(X,Y).
piece(L, C, X,Y).
%diffType(X,Y) :- piece(X,A,_,_,_),piece(Y,B,_,_,_),A \= B.
%ici cest side différents, si on veut le type diff alors rajouer X\=Y.

%new version :
%cas dans les angles
aCote1([X,Y],[U,V]) :- piece(0,0, X,Y), piece(1, 1, U,V).
aCote1([X,Y],[U,V]) :- piece(0,0, X,Y), piece(0,1,U,V).
aCote1([X,Y],[U,V]) :- piece(0,7,X,Y), piece(0,6,U,V).
aCote1([X,Y],[U,V]) :- piece(0,7,X,Y), piece(1,7,U,V).
aCote1([X,Y],[U,V]) :- piece(7,7,X,Y), piece(7,6,U,V).
aCote1([X,Y],[U,V]) :- piece(7,7,X,Y), piece(6,7,U,V).
aCote1([X,Y],[U,V]) :- piece(7,0,X,Y), piece(7,1,U,V).
aCote1([X,Y],[U,V]) :- piece(7,0,X,Y), piece(6,0,U,V).
%cas 1erederniere ligne colonne
aCote1([X,Y],[U,V]) :- piece(0,C,X,Y), piece(0, C-1,U,V).
aCote1([X,Y],[U,V]) :- piece(0,C, X,Y), piece(0, C+1, U,V).
aCote1([X,Y],[U,V]) :- piece(0,C, X,Y), piece(1, C, U,V).
aCote1([X,Y],[U,V]) :- piece(7, C, X,Y), piece(7, C-1, U,V).
aCote1([X,Y],[U,V]) :- piece(7, C, X,Y), piece(7, C+1, U,V).
aCote1([X,Y],[U,V]) :- piece(7, C, X,Y), piece(6, C, U,V).
aCote1([X,Y],[U,V]) :- piece(L, 0, X,Y), piece(L2, 0, U,V), L2 is L-1.
aCote1([X,Y],[U,V]) :- piece(L, 0, X,Y), piece(L2, 0, U,V), L2 is L+1.
aCote1([X,Y],[U,V]) :- piece(L, 0, X,Y), piece(L, 1, U,V).
aCote1([X,Y],[U,V]) :- piece(L, 7, X,Y), piece(L2, 7, U,V), L2 is L-1.
aCote1([X,Y],[U,V]) :- piece(L, 7, X,Y), piece(L2, 7,U,V), L2 is L+1.
aCote1([X,Y],[U,V]) :- piece(L, 7, X,Y), piece(L, 6, U,V).
%cas general
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(L, C, X,Y),piece(L2, C, U,V), L2 is L+1.
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(L, C, X,Y),piece(L2, C, U,V), L2 is L-1.
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(L,C, X,Y),piece(L, C2, U,V),C2 is C-1.
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(L,C,X,Y),piece(L, C2,U,V), C2 is C+1. 
%autres
aCote([X,Y],[U,V]) :- aCote1([X,Y],[U,V]). 
aCote([X,Y],[U,V]) :- aCote1([U,V],[X,Y]).



%frozen
frozen(X,W) :- aCote([X,W],[Y,Z]), inf(X,Y), piece(_,_,X,W), piece(_,_,Y,Z), W \= Z, \+ aCote([X,W],[A,W]), frozenTab(L),\+element([X,W],L).

%ajout au tableau des frozen
frozenTab([[X,Y]|L]) :- piece(_,_,X,Y), frozen(X,Y), frozenTab(L).

%etat --> faire une transition de in vers out si une piece est dans un trap --> ligne 2(enfin 72 plutôt)




%predicat trap
trap(X,Y) :- piece(2,2,X,Y). 
trap(X,Y) :- piece(5,2,X,Y).
trap(X,Y) :- piece(2,5,X,Y).
trap(X,Y) :- piece(5,5,X,Y).  

%ajout au tableau des capturés
captured([[X,Y]|L]) :- trap(X,Y), captured(L). 

%commentaires pour les déplacements
%Pour les pièces 4 directions possibles : forward, backward, left and right
%rabbits : peuvent pas backward
%Pas possible de faire un tour où tout revient à la même position que au début du tour
%Entre 1 et 4 steps par tour
%push/pull = 2 steps : by a stronger to a weaker opponents piece
%* A stronger piece can also freeze any opponents piece that is weaker than it. 
%A piece which is next to an opponents stronger piece is considered to be frozen and cannot move on its own; 
%though it can be pushed or pulled by opponents stronger pieces. 
%However if there is a friendly piece next to it the piece is unfrozen and is free to move. 
%* Idem pour les traps : si il y a une piece amie à côté alors ne tombe pas dans le trou

%architecture :
%move = get_move(board, state);
%move: 
%    steps: array of 4 move
%    move: from (row, col), piece, to (row, col)  (ex : [2,3, rabbit, gold,3,3])
    
    
 % Predicat gamestate
 gamestate(X, Y, Z, U) :-  U=<4 ,captured(Y), frozenTab(Z), remainSteps(U).
 % Les pièces capturées et frozen sont des listes?  La réponse est oui, en tout cas cest comme ça que je lai codé
 
 % Predicat remainSteps  //pas sure du tout
 remainSteps(0):-!.
 remainSteps(N) :-  N=<4, M is N-1, remainSteps(M).
 % En fait je vois pas linteret de faire de la récursivité : remainSteps(N) :- N>0,N<=4. 
 % Cest pour pouvoir lutiliser dans une boucle ou autre, mais tas peut être raison, ya moyen que ça soit inutile

concat([],L,L). 
concat([X|L1], L2,[X|L3]) :- concat(L1,L2,L3).

%prédicat Free (place libre) 
free(X,Y) :- \+ piece(_,_,X,Y). 

%retire lelement de la liste 
%retireElement(_, [], []).
%retireElement(X, [X|Q], Q) :- !.
%retireElement(X, [T|Q], [T|R]) :- retireElement(X, Q, R).

%supp toutes les occurences
suppr([],X,[]). 
suppr([X|Ist],X,SansX) :- suppr(Ist,X,SansX).
suppr([X|Ist],Z,[X|SansX]) :- Z \= X , suppr(Ist,Z,SansX).

%predicat board
board([[L,C,X,Y]|B]) :- piece(L,C,X,Y),  free(L,C), L=<7, L>=0, C=<7, L>=0, not(trap(X,Y)), board(B). 
 


%tentative pour faire en sorte que toutes les pieces trap soient éjectées du jeu (donc napparaissent plus dans board)
%board(b):-suppr([_,_,_,_],b1,b), board(b1).

%predicat possMove, en supposant silver en haut et gold en bas
%on peut pas bouger les out ou silver
%cas special des lapins qui peuvent pas aller backward

%jaurais bien mis des | entre les différents not(board(..)) non ?
possMove(_,_,[]).
possMove(rabbit,silver,[[[L,C],[L2, C]]|Res]) :-   piece(L, C,rabbit,silver), \+ board([_,[L2,C,_,_],_]), L2 is L-1, possMove(rabbit, silver, Res),!.
possMove(rabbit,silver,[[[L,C],[L,C2]]|Res]) :- piece(L,C, rabbit,silver),  \+ board([_,[L,C2,_,_],_]), C2 is C+1, possMove(rabbit, silver, Res),!.
possMove(rabbit,silver,[[[L,C],[L,C2]]|Res]) :- piece(L, C, rabbit,silver),  \+ board([_,[L,C2,_,_],_]), C2 is C-1, possMove(rabbit, silver, Res),!.
possMove(rabbit,gold,[[[L,C],[L2, C]]|Res]) :- piece(L, C, rabbit,gold),  \+ board([_,[L2,C,_,_],_]), L2 is L-1,possMove(rabbit, gold, Res),!.
possMove(rabbit,gold,[[[L,C],[L,C2]]|Res]) :- piece(L, C, rabbit,gold), \+ board([_,[L,C2,_,_],_]), C2 is C+1, possMove(rabbit, gold, Res),!.
possMove(rabbit,gold,[[[L,C],[L,C2]]|Res]) :- piece(L, C, rabbit,gold), \+ board([_,[L,C2,_,_],_]), C2 is C-1,possMove(rabbit, gold, Res),!.
possMove(X,Y,[[[L,C],[L2, C]]|Res]) :-  piece(L, C, X,Y), X \= rabbit , \+ board([_,[L2,C,_,_],_]), L2 is L-1, possMove(X,Y,Res),!.
possMove(X,Y,[[[L,C],[L,C2]]|Res]) :- piece(L, C, X,Y), X \= rabbit , \+ board([_,[L,C2,_,_],_]),C2 is C+1, possMove(X,Y, Res),!.
possMove(X,Y,[[[L,C],[L,C2]]|Res]) :- piece(L, C, X,Y), X \= rabbit , \+ board([_,[L,C2,_,_],_]),C2 is C-1, possMove(X,Y, Res),!.
possMove(X,Y,[[[L,C],[L2,C]]|Res]) :- piece(L, C, X,Y), X \= rabbit , \+ board([_,[L2,C,_,_],_]),L2 is L+1, possMove(X,Y, Res),!. 

%sens du mouvement demandé
sens(S) :- sens(gauche).
sens(S) :- sens(droite).
sens(S) :- sens(bas).
sens(S) :- sens(haut). 

% X : pièce poussant(son type), W : pièce poussée(son type), N : nombre de coup restant, (L, C) :position de la piece à pousser
%choix du sens
possPush(X,silver,W,N,L,C,S) :- !,N>=2, (L+1)=<7,  free(L2,C), L2 is L+1,sens(bas), piece(L, C, W,gold,L,C,in),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :-!, N>=2, (C+1)=<7,  free(L,C2), C2 is C+1,sens(droite), piece(L, C, W, gold),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :-!, N>=2,(C-1)>=0,  free(L,C2), C2 is C-1,sens(gauche),  piece(L, C, W, gold),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :- !,N>=2, (L-1)>=0,  free(L2,C), L2 is L-1,sens(haut),  piece(L, C, W, gold),aCote(X,W),inf(W,X).

possPush(X,gold,W,N,L,C,S) :-!, N>=2, (L+1)=<7,  free(L2,C), L2 is L+1,sens(bas), piece(L, C, W, silver),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :-!, N>=2, (C+1)=<7,  free(L,C2), C2 is C+1,sens(droite) ,piece(L, C, W, silver),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :-!, N>=2, (C-1)>=0,  free(L,C2), C2 is C-1,sens(gauche)  ,piece(L, C, W, silver),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :- !,N>=2, (L-1)>=0,  free(L2,C), L2 is L-1, sens(haut), piece(L, C, W, silver),aCote(X,W),inf(W,X).

%Comment appliquer le mouvement ? Vraiment pas sure de ce qui suit, parce quon ne demande pas si lutilisateur VEUT déplacer la pièce
%comment changer létat ?
%je pense quon peut utiliser oppSide() au lieu de toujours utiliser silver ou gold et faire 15 lignes, à voir demain
%Jai fait un predicat qui normalement change létat automatiquement (cf plus haut l. 72)

%piece(W,gold,L2,C,_) :- possPush(X,silver,W,N,L,C,bas), L2 is L+1. 
%piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,bas). 

%piece(W,gold,L2,C,_) :- possPush(X,silver,W,N,L,C,haut), L2 is L-1. 
%piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,haut). 

%piece(W,gold,L,C2,_) :- possPush(X,silver,W,N,L,C,droite), C2 is C+1. 
%piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,droite). 

%piece(W,gold,L,C2,_) :- possPush(X,silver,W,N,L,C,gauche), C2 is C-1. 
%piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,gauche). 

%piece(W,silver,L2,C,_) :- possPush(X,gold,W,N,L,C,bas),L2 is L+1. 
%piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,bas). 

%piece(W,silver,L2,C,_) :- possPush(X,gold,W,N,L,C,haut), L2 is L-1. 
%piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,haut). 

%piece(W,silver,L,C2,_) :- possPush(X,gold,W,N,L,C,droite), C2 is C+1. 
%piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,droite). 

%piece(W,silver,L,C2,_) :- possPush(X,gold,W,N,L,C,gauche), C2 is C-1. 
%piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,gauche). 

%faire aussi vers le haut/colonnes etc, toute direction
%on ne peut pas déloger ses propres pièces
possPull(X,silver,W,N,L,C) :- N>=2,piece(L, C, W,gold),piece(L2,C,X,silver),free(L3,C), L2 is L-1, L3 is L-2, L-2>=0,inf(W,X). 
possPull(X,gold,W,N,L,C) :- N>=2,piece(L, C, W,silver),piece(L2,C,X,gold),free(L3,C), L2 is L+1, L3 is L+2, L+2=<7,inf(W,X).
possPull(X,gold,W,N,L,C) :- N>=2,piece(L, C, W,silver),piece(L,C2,X,gold),free(L,C3), C2 is C+1, C3 is C+2, C+2=<7,inf(W,X).
possPull(X,gold,W,N,L,C) :- N>=2,piece(L, C, W,silver),piece(L, C2, X,gold),free(L,C3), C2 is C-1, C3 is C-2, C-2>=0,inf(W,X).

%piece(W,gold,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-1. 
%piece(X,silver,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-2. 

%piece(W,silver,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+1. 
%piece(X,gold,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+2. 

%predicat formation base de données 
inserer([]).
inserer([X|L]):- asserta(X), inserer(L). 


%predicat Get_Move, on ajoute un move au tableau :
moves([]).
moves([[[L1,C1],[L2,C2]]|L]) :- move([L1,C1],[L2,C2]), moves(L).
get_moves(M, Gamestate, Board) :-  retractall(piece(_)), inserer(Board), moves(M).
%utiliser un predicat tailleListe pour verifier <=4

%predicat move
%faire un predicat choix pour que le joueur choisisse parmi les possmove et les possPull/possPush et no_move ?
%on est obligé de faire au moins un mouvement
%P1=Position 1 [L1,C1] et P2=Position 2 [L2,C2]
%mais on peut garder L1,C1 et L2,C2 séparés si on veut


%move([L1,C1],[L2,C2]):- possMove(_,_,[[[L1,C1],[L2,C2]]|Res]). %voir comment on dit que [L1,C1],[L2,C2] est un des mouvements possibles de possMove
%move([L1,C1],[L2,C2]) :- L=:=L1+1,L2 is L1-1, possPull(_,_,_,_,L,C).
%move([L1,C1],[L2,C2]) :-L=:=L1-1,L2 is L1+1, possPull(_,_,_,_,L,C).
%move([L1,C1],[L2,C2]) :- C=:=C1+1,C2 is C1-1, possPull(_,_,_,_,L,C).
%move([L1,C1],[L2,C2]) :- C=:=C1-1,C2 is C1+1, possPull(_,_,_,_,L,C).

%move([L1,C1],[L2,C2]) :- L=:=L1+1,L2 is L1+1, possPush(_,_,_,_,L,C,_).
%move([L1,C1],[L2,C2]) :- L=:=L1-1,L2 is L1-1, possPush(_,_,_,_,L,C,_).
%move([L1,C1],[L2,C2]) :- C=:=C1+1,C2 is C1+1, possPush(_,_,_,_,L,C,_).
%move([L1,C1],[L2,C2]) :- C=:=C1-1,C2 is C1-1, possPush(_,_,_,_,L,C,_).


possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- L=:=L1+1,L2 is L1-1, possPull(_,_,_,_,L,C), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :-L=:=L1-1,L2 is L1+1, possPull(_,_,_,_,L,C), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]):- C=:=C1+1,C2 is C1-1, possPull(_,_,_,_,L,C), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- C=:=C1-1,C2 is C1+1, possPull(_,_,_,_,L,C), possmove(_,_,S),!.

possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- L=:=L1+1,L2 is L1+1, possPush(_,_,_,_,L,C,_), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- L=:=L1-1,L2 is L1-1, possPush(_,_,_,_,L,C,_), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- C=:=C1+1,C2 is C1+1, possPush(_,_,_,_,L,C,_), possmove(_,_,S),!.
possmove(_,_,[[[L1,C1],[L2,C2]]|S]) :- C=:=C1-1,C2 is C1-1, possPush(_,_,_,_,L,C,_), possmove(_,_,S), !.
!
move([L1,C1],[L2,C2]):- possMove(_,_,[[[L1,C1],[L2,C2]]|Res]), !.

piece(L2,C2,_,_) :- move([L1,C1],[L2,C2]), piece(L1,C1,_,_). 
% à la place de tout le reste avec les possPush et PossPull ?




consult(arimaa.pl).
