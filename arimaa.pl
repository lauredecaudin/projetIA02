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
get_moves([[[1,0],[2,0]],[[0,0],[1,0]],[[0,1],[0,0]],[[0,0],[0,1]]], Gamestate, Board).


/* Faut lire le README du Git de P16, ya pleins de prédicats utiles dont on pourrait peut être s'inspirer*/ 
https://github.com/vincebhx/IA02-Khan/blob/master/README.md

/*Git du projet*/
/* https://github.com/rlacazel/Prolog-Arimaa  */



% predicat inferiorite
inf(X,Y) :- inf1(X,Y).
inf(X,Y) :- inf1(X,Z), inf(Z,Y).
inf1(rabbit, cat).
inf1(cat, dog).
inf1(dog, horse).
inf1(horse, camel).
inf1(camel, elephant).

%faire predicat strength je pense, un peu de la même maniere, tentative :
strength(rabbit, 1).
%non en fait a part attribuer strength(cat, 2). strength(dog, 3). etc je vois pas
strength(rabbit, 1).
strength(cat, 2).
strength(dog, 3).
strength(horse, 4).
strength(camel, 5).
strength(elephant, 6).


%predicat type
type(X) :- type(rabbit) | type(cat) | type(dog) | type(horse) | type(camel) | type(elephant).


move([[],[]], L, L).



/*Petit prédicat pour trouver le joueur adverse*/
oppSide(silver, gold).
oppSide(gold, silver).

%predicat qui donne la couleur de notre piece ?
side(X) :- side(silver) | side(gold).
side(X) :- oppSide(X,_).  

%Une pièce est définie par un tuple piece(type,side,Lin,Col,Etat), où :
    Après en soit "strength" est déterminé par "type" donc devient "inutile"
    (Col, Lin) est la position de la pièce sur le plateau. (plateau de 8x8)
    Etat détermine si la pièce est en jeu, si elle est en jeu et est frozen, ou si elle est hors jeu (dans un piège); Etat peut prendre les valeurs 'in', 'frozen' ou 'out'.

%etat --> faire une transition de in vers out si une piece est dans un trap --> ligne 2(enfin 72 plutôt)
etat(X) :- etat(in) | etat(frozen) | etat(out).
piece(X,Y,_,_,out):- (piece(X,Y,_,_,in)|piece(X,Y,_,_,frozen)), trap(X,Y), not(aCote([X,Y],[W,Y])),.
% pas d'accord car si tu as un ami à coté de toi tu ne disparait pas. --> proposition au dessus
piece(X,Y,_,_,frozen) :- piece(X,Y,_,_,in), frozen(X,Y).

position(X,Y).
piece(X,Y,L,C,E):-type(X),side(Y),position(L,C),etat(E).
diffType(X,Y) :- piece(X,A,_,_,_),piece(Y,B,_,_,_),A \== B. %ici c'est side différents, si on veut le type diff alors rajouer X\==Y.

%predicat trap
trap(X,Y) :- piece(X,Y,2,2,_) | piece(X,Y,5,2,_) | piece(X,Y,2,5,_) | piece(X,Y,5,5,_).  
%(ici X est le type, et Y side)

%ajout au tableau des capturés
captured([[X,Y]|L]) :- trap(X,Y), captured(L). 

%a faire plus tard
%ajout au tableau des frozen
frozenTab([[X,Y]|L]) :- piece(X,Y,_,_,frozen), frozenTab(L).

%commentaires pour les déplacements
%Pour les pièces 4 directions possibles : forward, backward, left and right
%rabbits : peuvent pas backward
%Pas possible de faire un tour où tout revient à la même position que au début du tour
%Entre 1 et 4 steps par tour
%push/pull = 2 steps : by a stronger to a weaker opponent's piece
%* A stronger piece can also freeze any opponent's piece that is weaker than it. 
%A piece which is next to an opponent's stronger piece is considered to be frozen and cannot move on its own; 
%though it can be pushed or pulled by opponents stronger pieces. 
%However if there is a friendly piece next to it the piece is unfrozen and is free to move. 
%* Idem pour les traps : si il y a une piece amie à côté alors ne tombe pas dans le trou

%architecture :
%move = get_move(board, state);
%move: 
%    steps: array of 4 move
%    move: from (row, col), piece, to (row, col)  (ex : [2,3, rabbit, gold,3,3])
    
    
 % Predicat gamestate
 gamestate(X, Y, Z, U) :- side(X), captured(Y), frozen(Z), remainSteps(U), U<=4.
 % Les pièces capturées et frozen sont des listes?  La réponse est oui, en tout cas c'est comme ça que je l'ai codé
 
 % Predicat remainSteps  //pas sure du tout
 remainSteps(0):-!.
 remainSteps(N) :-  M>0, M is N+1, remainSteps(M).
 % Je crois qu'on peut enlever le M>0, si on met M>0. 
 % En fait je vois pas l'interet de faire de la récursivité : remainSteps(N) :- N>0,N<=4. 
 % C'est pour pouvoir l'utiliser dans une boucle ou autre, mais t'as peut être raison, ya moyen que ça soit inutile

concat([],L,L). 
concat([X|L1], L2,[X|L3]) :- concat(L1,L2,L3).

%prédicat Free (place libre) 
notFree(X,Y) :- piece(_,_,X,Y,_).
% attention, ce n'est pas juste en mettant le mot "not" dans le nom du prédicat que tu vas créer une négation
%----> je sais mais la c'est pas libre s'il y a une place. y a pas de négation ici ? 

%il faut créer un prédicat normal p(X,Y), et ensuite tu peux appeler sa négation(en gros dire que c'est pas vérifié) dans 
%une règle pour un autre prédicat :
free(X,Y) :- not(piece(_,_,X,Y,_). 
%diapo101 du poly

%predicat board
board([[L,C,X,Y]|L]) :- board(L), piece(X,Y,L,C,in|frozen), L<=7, L>=0, C<=7, L>=0, not trap(X,Y), free(L,C).  
//concaténation ? j'aurais bien ajouter E. : board([[T|Q],[L,C,X,Y,E]]) :- board([T|Q]), piece(X,Y,L,C,E),E == in|frozen, L<=7, L>=0, C<=7, L>=0, not trap(X,Y)
 
%predicat possMove, en supposant silver en haut et gold en bas
%on ne peut pas bouger les out ou silver
%cas special des lapins qui ne peuvent pas aller backward
%Pourquoi board([[_],[L+1,C]],_,_) et pas board([[_],[L+1,C,_,_],[_]])?   Je pense que tu as raison...

possMove(rabbit,silver,[[[L,C],[L+1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]]]) :- piece(rabbit,silver,L,C,in), not board([[_],[L+1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_).
possMove(rabbit,gold,[[[L,C],[L-1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]]]) :- piece(rabbit,gold,L,C,in), not board([[_],[L-1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_).
possMove(X,Y,[[[L,C],[L-1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]],[[L,C],[L+1,C]]]) :- piece(X,Y,L,C,in), X \== rabbit , not board([[_],[L-1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_), not board([[_],[L+1,C]],_,_). 

aCote1(X,Y) :- piece(X,_,L,C,_),piece(Y,_,L+1,C,_)|piece(Y,_,L-1,C,_)|piece(Y,_,L,C-1,_)|piece(Y,_,L,C+1,_). 

%new version :
aCote(X,Y) :- aCote1(X,Y). 
aCote(X,Y) :- aCote1(Y,X). 
%cas dans les angles
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,0 ,_), piece(U,V,1,1,_)|piece(U,V,0,1,_),!.
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,7 ,_), piece(U,V,0,6,_)|piece(U,V,1,7,_),!.
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,7 ,_), piece(U,V,7,6,_)|piece(U,V,6,7,_), !.
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,0 ,_), piece(U,V,7,1,_)|piece(U,V,6,0,_),!.
%cas 1ere/derniere ligne/colonne
aCote1([X,Y],[U,V]) :- piece(X,Y, 0,C ,_), piece(U,V,0,C-1,_)|piece(U,V,0,C+1,_)|piece(U,V,1,C,_),!.
aCote1([X,Y],[U,V]) :- piece(X,Y, 7,C ,_), piece(U,V,7,C-1,_)|piece(U,V,7,C+1,_)|piece(U,V,6,C,_),!.
aCote1([X,Y],[U,V]) :- piece(X,Y, L,0 ,_), piece(U,V,L-1,0,_)|piece(U,V,L+1,0,_)|piece(U,V,L,1,_),!.
aCote1([X,Y],[U,V]) :- piece(X,Y, L,7 ,_), piece(U,V,L-1,7,_)|piece(U,V,L+1,7,_)|piece(U,V,L,6,_),!.
%cas general
aCote1([X,Y], [U,V]) :- piece(X,Y,L,C,_),piece(U,V,L+1,C,_)|piece(U,V,L-1,C,_)|piece(U,V,L,C-1,_)|piece(U,V,L,C+1,_), L>=1, C>=1, L<=6, C<=6.

not(P) :- P, !, fail.
not(_).

%sens du mouvement demandé
sens(S) :- sens(gauche) | sens(droite) | sens(bas) | sens(haut). 

% X : pièce poussant(son type), W : pièce poussée(son type), N : nombre de coup restant, (L, C) :position de la piece à pousser
%j'ai mis >=2 plutôt
%j'ai rajouté les contraintes pour que le voisin soit pas hors_board

possPush(X,silver,W,N,L,C,S) :- N>=2,(free(L+1,C),sens(bas), L+1<=7)|(free(L,C+1),sens(droite), C+1<=7)|(free(L,C-1),sens(gauche), C-1>=0)|(free(L-1,C),sens(haut), L-1>=0),piece(W,gold,L,C,in),aCote(X,W),inf(W,X). 
possPush(X,gold,W,N,L,C,S) :- N>=2,(free(L+1,C),sens(bas), L+1<=7)|(free(L,C+1),sens(droite),C+1<=7)|(free(L,C-1),sens(gauche), C-1>=0)|(free(L-1,C),sens(haut), L-1>=0),piece(W,silver,L,C,in),aCote(X,W),inf(W,X).

%Comment appliquer le mouvement ? Vraiment pas sure de ce qui suit, parce qu'on ne demande pas si l'utilisateur VEUT déplacer la pièce
%comment changer l'état ?
%je pense qu'on peut utiliser oppSide() au lieu de toujours utiliser silver ou gold et faire 15 lignes, à voir demain
%J'ai fait un predicat qui normalement change l'état automatiquement (cf plus haut l. 72)

piece(W,gold,L2,C,_) :- possPush(X,silver,W,N,L,C,bas), L2 is L+1. 
piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,bas). 

piece(W,gold,L2,C,_) :- possPush(X,silver,W,N,L,C,haut), L2 is L-1. 
piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,haut). 

piece(W,gold,L,C2,_) :- possPush(X,silver,W,N,L,C,droite), C2 is C+1. 
piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,droite). 

piece(W,gold,L,C2,_) :- possPush(X,silver,W,N,L,C,gauche), C2 is C-1. 
piece(X,silver,L,C,_) :- possPush(X,silver,W,N,L,C,gauche). 

piece(W,silver,L2,C,_) :- possPush(X,gold,W,N,L,C,bas),L2 is L+1. 
piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,bas). 

piece(W,silver,L2,C,_) :- possPush(X,gold,W,N,L,C,haut), L2 is L-1. 
piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,haut). 

piece(W,silver,L,C2,_) :- possPush(X,gold,W,N,L,C,droite), C2 is C+1. 
piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,droite). 

piece(W,silver,L,C2,_) :- possPush(X,gold,W,N,L,C,gauche), C2 is C-1. 
piece(X,gold,L,C,_) :- possPush(X,gold,W,N,L,C,gauche). 

%frozen
frozen(X,W) :- aCote(X,Y), inf(X,Y), piece(X,W,_,_,in), piece(Y,Z,_,_,in), W \== Z, not (aCote(X,A), piece(A,W,_,_,in)). 

possPull(X,silver,W,N,L,C) :- N>=2,piece(W,gold,L,C,in),piece(X,silver,L-1,C,in),free(L-2,C), L-2>=0,inf(W,X). 
possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L+1,C,in),free(L+2,C), L+2<=7,inf(W,X).

piece(W,gold,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-1. 
piece(X,silver,L2,C,_) :- possPull(X,silver,W,N,L,C), L2 is L-2. 

piece(W,silver,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+1. 
piece(X,gold,L2,C,_) :- possPull(X,gold,W,N,L,C), L2 is L+2. 

%pourquoi ne pas faire selon les colonnes aussi ? : 
%parce que dans les règles tu ne peux attirer que vers toi si tu as d'abord reculé

possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L,C+1,in),free(L,C+2), C+2<=7,inf(W,X).
possPull(X,gold,W,N,L,C) :- N>=2,piece(W,silver,L,C,in),piece(X,gold,L,C-1,in),free(L,C-2), C-2>=0,inf(W,X).

%predicat Get_Move, on ajoute un move au tableau :
get_moves(_, [_,_,_,0],_ ):- !.  %si plus de step possible, on arrête
get_moves([[[L1,C1],[L2,C2]]|L], Gamestate, Board) :- get_moves(L,Gamestate, Board), move([L1,C1],[L2,C2]).
% euhhhh moi pas comprendre, moi bête

%predicat move
%faire un predicat choix pour que le joeur choisisse parmi les possmove et les possPull/possPush et no_move ?
%on est obligé de faire au moins un mouvement
%P1=Position 1 [L1,C1] et P2=Position 2 [L2,C2]
%mais on peut garder L1,C1 et L2,C2 séparés si on veut
move(L1,C1, X, Y,L2,C2):- possMove(X,Y,[[_],[[L1,C1],[L2,C2]],[_]]). %voir comment on dit que [L1,C1],[L2,C2] est un des mouvements possibles de possMove
move(L1,C1,X,Y,L2,C2) :- ((L==L1+1,L2 is L1-1)| (L==L1-1,L2 is L1+1)|(C==C1+1,C2 is C1-1)|(C==C1-1,C2 is C1+1)), possPull(X,_,_,_,L,C).
move(L1,C1,X,Y,L2,C2) :- ((L==L1+1,L2 is L1+1)| (L==L1-1,L2 is L1-1)|(C==C1+1,C2 is C1+1)|(C==C1-1,C2 is C1-1)), possPush(X,_,_,_,L,C,_).

consult(arimaa.pl).

