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

% Faut lire le README du Git de P16, ya pleins de prédicats utiles dont on pourrait peut être s'inspirer*/ 
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


%Une pièce est définie par un tuple piece(type,side,Lin,Col,Etat), où :
%  Après en soit trength est déterminé par type donc devient inutile
% (Col, Lin) est la position de la pièce sur le plateau. (plateau de 8x8)
%  Etat détermine si la pièce est en jeu, si elle est en jeu et est , ou si elle est hors jeu (dans un piège); Etat peut prendre les valeurs 'in', 'frozen' ou 'out'.

%ici je trouve ça bizarre le position...
position(X,Y).
piece(X,Y,L,C,E).
diffType(X,Y) :- piece(X,A,_,_,_),piece(Y,B,_,_,_),A \= B.
%ici cest side différents, si on veut le type diff alors rajouer X\=Y.

%new version :
%cas dans les angles
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,0 ,_), piece(U,V,1,1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,0 ,_), piece(U,V,0,1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,7 ,_), piece(U,V,0,6,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,7 ,_), piece(U,V,1,7,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,7 ,_), piece(U,V,7,6,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,7 ,_), piece(U,V,6,7,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,0 ,_), piece(U,V,7,1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,0 ,_), piece(U,V,6,0,_).
%cas 1erederniere ligne colonne
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,C ,_), piece(U,V,0,C-1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,C ,_), piece(U,V,0,C+1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,C ,_), piece(U,V,1,C,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,C ,_), piece(U,V,7,C-1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,C ,_), piece(U,V,7,C+1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,C ,_), piece(U,V,6,C,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,0 ,_), piece(U,V,L-1,0,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,0 ,_), piece(U,V,L+1,0,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,0 ,_), piece(U,V,L,1,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,7 ,_), piece(U,V,L-1,7,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,7 ,_), piece(U,V,L+1,7,_).
aCote1([X,Y],[U,V]) :- piece(X,Y, L,7 ,_), piece(U,V,L,6,_).
%cas general
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(X,Y,L,C,_),piece(U,V,L+1,C,_).
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(X,Y,L,C,_),piece(U,V,L-1,C,_).
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(X,Y,L,C,_),piece(U,V,L,C-1,_).
aCote1([X,Y], [U,V]) :- L>=1, C>=1, L=<6, C=<6, piece(X,Y,L,C,_),piece(U,V,L,C+1,_). 
%autres
aCote([X,Y],[U,V]) :- aCote1([X,Y],[U,V]). 
aCote([X,Y],[U,V]) :- aCote1([U,V],[X,Y]).



%frozen
frozen(X,W) :- aCote([X,W],[Y,Z]), inf(X,Y), piece(X,W,_,_,in), piece(Y,Z,_,_,in), W \= Z, \+ aCote([X,W],[A,W]).

%ajout au tableau des frozen
frozenTab([[X,Y]|L]) :- piece(X,Y,_,_,frozen), frozenTab(L).

%etat --> faire une transition de in vers out si une piece est dans un trap --> ligne 2(enfin 72 plutôt)


piece(X,Y,_,_,out):- trap(X,Y), \+ aCote([X,Y],[W,Y]), piece(X,Y,_,_,in).
piece(X,Y,_,_,out):- trap(X,Y), \+ aCote([X,Y],[W,Y]), piece(X,Y,_,_,frozen).
piece(X,Y,_,_,frozen) :- frozen(X,Y), piece(X,Y,_,_,in).


%predicat trap
trap(X,Y) :- piece(X,Y,2,2,_). 
trap(X,Y) :- piece(X,Y,5,2,_).
trap(X,Y) :- piece(X,Y,2,5,_).
trap(X,Y) :- piece(X,Y,5,5,_).  

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
free(X,Y) :- \+ piece(_,_,X,Y,_). 

%retire lelement de la liste 
%retireElement(_, [], []).
%retireElement(X, [X|Q], Q) :- !.
%retireElement(X, [T|Q], [T|R]) :- retireElement(X, Q, R).

%supp toutes les occurences
suppr([],X,[]). 
suppr([X|Ist],X,SansX) :- suppr(Ist,X,SansX).
suppr([X|Ist],Z,[X|SansX]) :- Z \= X , suppr(Ist,Z,SansX).

%predicat board
board([[L,C,X,Y,in]|B]) :- piece(X,Y,L,C,in),  free(L,C), L=<7, L>=0, C=<7, L>=0, not(trap(X,Y)), board(B). 
board([[L,C,X,Y,frozen]|B]) :- piece(X,Y,L,C,frozen), free(L,C), L=<7, L>=0, C=<7, L>=0, not(trap(X,Y)),  board(B). 


%tentative pour faire en sorte que toutes les pieces trap soient éjectées du jeu (donc napparaissent plus dans board)
%board(b):-suppr([_,_,_,_,out],b1,b), board(b1).

%predicat possMove, en supposant silver en haut et gold en bas
%on peut pas bouger les out ou silver
%cas special des lapins qui peuvent pas aller backward

%jaurais bien mis des | entre les différents not(board(..)) non ?
possMove(_,_,[]).
possMove(rabbit,silver,[[[L,C],[L+1, C]]|Res]) :- piece(rabbit,silver,L,C,in), \+ board([_,[L+1,C,_,_,_],_]), possMove(rabbit, silver, Res).
possMove(rabbit,silver,[[[L,C],[L,C+1]]|Res]) :- piece(rabbit,silver,L,C,in), \+ board([_,[L,C+1,_,_,_],_]), possMove(rabbit, silver, Res).
possMove(rabbit,silver,[[[L,C],[L,C-1]]|Res]) :- piece(rabbit,silver,L,C,in), \+ board([_,[L,C-1,_,_,_],_]), possMove(rabbit, silver, Res).
possMove(rabbit,gold,[[[L,C],[L-1, C]]|Res]) :- piece(rabbit,gold,L,C,in), \+ board([_,[L-1,C,_,_,_],_]), possMove(rabbit, gold, Res).
possMove(rabbit,gold,[[[L,C],[L,C+1]]|Res]) :- piece(rabbit,gold,L,C,in),\+ board([_,[L,C+1,_,_,_],_]), possMove(rabbit, gold, Res).
possMove(rabbit,gold,[[[L,C],[L,C-1]]|Res]) :- piece(rabbit,gold,L,C,in), \+ board([_,[L,C-1,_,_,_],_]), possMove(rabbit, gold, Res).
possMove(X,Y,[[[L,C],[L-1, C]]|Res]) :- piece(X,Y,L,C,in), X \= rabbit , \+ board([_,[L-1,C,_,_,_],_]), possMove(X,Y,Res).
possMove(X,Y,[[[L,C],[L,C+1]]|Res]) :- piece(X,Y,L,C,in), X \= rabbit , \+ board([_,[L,C+1,_,_,_],_]), possMove(X,Y, Res).
possMove(X,Y,[[[L,C],[L,C-1]]|Res]) :- piece(X,Y,L,C,in), X \= rabbit , \+ board([_,[L,C-1,_,_,_],_]), possMove(X,Y, Res).
possMove(X,Y,[[[L,C],[L+1,C]]|Res]) :- piece(X,Y,L,C,in), X \= rabbit , \+ board([_,[L+1,C,_,_,_],_]), possMove(X,Y, Res). 

%sens du mouvement demandé
sens(S) :- sens(gauche).
sens(S) :- sens(droite).
sens(S) :- sens(bas).
sens(S) :- sens(haut). 

% X : pièce poussant(son type), W : pièce poussée(son type), N : nombre de coup restant, (L, C) :position de la piece à pousser
%choix du sens
possPush(X,silver,W,N,L,C,S) :- N>=2, (L+1)=<7, free(L+1,C),sens(bas), piece(W,gold,L,C,in),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :- N>=2, (C+1)=<7,free(L,C+1),sens(droite), piece(W,gold,L,C,in),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :- N>=2,(C-1)>=0,free(L,C-1),sens(gauche),  piece(W,gold,L,C,in),aCote(X,W),inf(W,X).
possPush(X,silver,W,N,L,C,S) :- N>=2, (L-1)>=0, free(L-1,C),sens(haut),  piece(W,gold,L,C,in),aCote(X,W),inf(W,X).

possPush(X,gold,W,N,L,C,S) :- N>=2, (L+1)=<7, free(L+1,C),sens(bas), piece(W,silver,L,C,in),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :- N>=2, (C+1)=<7, free(L,C+1),sens(droite) ,piece(W,silver,L,C,in),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :- N>=2, (C-1)>=0, free(L,C-1),sens(gauche)  ,piece(W,silver,L,C,in),aCote(X,W),inf(W,X).
possPush(X,gold,W,N,L,C,S) :- N>=2, (L-1)>=0, free(L-1,C),sens(haut), piece(W,silver,L,C,in),aCote(X,W),inf(W,X).

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
possPull(X,silver,W,N,L,C) :- N>=2,piece(W,gold,L,C,in),piece(X,silver,L-1,C,in),free(L-2,C), L-2>=0,inf(W,X). 
possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L+1,C,in),free(L+2,C), L+2=<7,inf(W,X).
possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L,C+1,in),free(L,C+2), C+2=<7,inf(W,X).
possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L,C-1,in),free(L,C-2), C-2>=0,inf(W,X).

%piece(W,gold,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-1. 
%piece(X,silver,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-2. 

%piece(W,silver,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+1. 
%piece(X,gold,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+2. 

%predicat Get_Move, on ajoute un move au tableau :
get_moves(_, [_,_,_,0],_ ):- !.  %si plus de step possible, on arrête
get_moves([[[L1,C1],[L2,C2]]|L], Gamestate, Board) :-  move([L1,C1],[L2,C2]), get_moves(L,Gamestate, Board).

%predicat move
%faire un predicat choix pour que le joueur choisisse parmi les possmove et les possPull/possPush et no_move ?
%on est obligé de faire au moins un mouvement
%P1=Position 1 [L1,C1] et P2=Position 2 [L2,C2]
%mais on peut garder L1,C1 et L2,C2 séparés si on veut

move([L1,C1],[L2,C2]):- possMove(_,_,[[[L1,C1],[L2,C2]]|Res]). %voir comment on dit que [L1,C1],[L2,C2] est un des mouvements possibles de possMove
move([L1,C1],[L2,C2]) :- L=:=L1+1,L2 is L1-1, possPull(_,_,_,_,L,C).
move([L1,C1],[L2,C2]) :-L=:=L1-1,L2 is L1+1, possPull(_,_,_,_,L,C).
move([L1,C1],[L2,C2]) :- C=:=C1+1,C2 is C1-1, possPull(_,_,_,_,L,C).
move([L1,C1],[L2,C2]) :- C=:=C1-1,C2 is C1+1, possPull(_,_,_,_,L,C).

move([L1,C1],[L2,C2]) :- L=:=L1+1,L2 is L1+1, possPush(_,_,_,_,L,C,_).
move([L1,C1],[L2,C2]) :- L=:=L1-1,L2 is L1-1, possPush(_,_,_,_,L,C,_).
move([L1,C1],[L2,C2]) :- C=:=C1+1,C2 is C1+1, possPush(_,_,_,_,L,C,_).
move([L1,C1],[L2,C2]) :- C=:=C1-1,C2 is C1-1, possPush(_,_,_,_,L,C,_).

piece(_,_,L2,C2,_) :- move([L1,C1],[L2,C2]), piece(_,_,L1,C1,_). 
% à la place de tout le reste avec les possPush et PossPull ?

consult(arimaa.pl).

