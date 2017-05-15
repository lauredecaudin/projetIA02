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

etat(X) :- etat(in) | etat(frozen) | etat(out).
position(X,Y).
piece(X,Y,L,C,E):-type(X),side(Y),position(L,C),etat(E).

%predicat trap
trap(X,Y) :- piece(X,Y,2,2,_) | piece(X,Y,5,2,_) | piece(X,Y,2,5,_) | piece(X,Y,5,5,_).  
%(ici X est le type, et Y side)

%ajout au tableau des capturés
captured([[T|Q],X,Y]) :- trap(X,Y), captured([T|Q]). 

%a faire plus tard
%ajout au tableau des frozen
frozen([[T|Q],X,Y]) :- ?(X,Y), frozen([T|Q]).

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
    
    
 %predicat gamestate
 gamestate(X, Y, Z, U) :- side(X), captured(Y), frozen(Z), remainSteps(U), U<=4.
 %les pièces capturées et frozen sont des listes?
 
 %predicat remainSteps  //pas sure du tout
 remainSteps(0).
 remainSteps(N) :-  N>0, M is N-1, remainSteps(M).
%en fait je vois pas l'interet de faire de la récursivité : remainSteps(N) :- N>0,N<=4. 

concat([],L,L). 
concat([X|L1], L2,[X|L3]) :- concat(L1,L2,L3).

%prédicat Free (place libre) 
notFree(X,Y) :- piece(_,_,X,Y,_).   
%j'aurais plutot fait Free(X,Y) :- not(piece(_,_,X,Y,_), car pour le coup c'est pas le fait de mettre "not" dans le nom du prédicat qui
%marche, mais c'est de faire un prédicat p(X,Y) et d'appeler not(p(X,Y)) dans les règles lors de la déclaration d'un autre prédicat ;)

%diapo101 du poly

%predicat board
board([[T|Q],[L,C,X,Y]]) :- board([T|Q]), piece(X,Y,L,C,in|frozen), L<=7, L>=0, C<=7, L>=0, not trap(X,Y).  
//est ce qu'il faut qu'on se démerde pour vérifier qu'aucune piece n'est présente à la même position ?
//concaténation ? j'aurais bien ajouter E. : board([[T|Q],[L,C,X,Y,E]]) :- board([T|Q]), piece(X,Y,L,C,E),E == in|frozen, L<=7, L>=0, C<=7, L>=0, not trap(X,Y)
 
%predicat possMove, en supposant silver en haut et gold en bas
%on ne peut pas bouger les out ou silver
%cas special des lapins qui ne peuvent pas aller backward
%Pourquoi board([[_],[L+1,C]],_,_) et pas board([[_],[L+1,C,_,_],[_]])?

possMove(rabbit,silver,[[[L,C],[L+1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]]]) :- piece(rabbit,silver,L,C,in), not board([[_],[L+1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_).
possMove(rabbit,gold,[[[L,C],[L-1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]]]) :- piece(rabbit,gold,L,C,in), not board([[_],[L-1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_).
possMove(X,Y,[[[L,C],[L-1, C]],[[L,C],[L,C+1]],[[L,C],[L,C-1]],[[L,C],[L+1,C]]]) ;- piece(X,Y,L,C,in), X \== rabbit , not board([[_],[L-1,C]],_,_), not board([[_],[L,C+1]],_,_), not board([[_],[L,C-1]],_,_), not board([[_],[L+1,C]],_,_). 

%Y : pièce poussant, X : type en cours, W : pièce poussée, N : nombre de coup restant, 
possPush(X,silver,W,N,L,C) :- free(L+1,C),piece(W,gold,L,C,in),piece(X,silver,L+1,C,in)|piece(X,silver,L-1,C,in)|piece(X,silver,L,C+1,in)|piece(X,silver,L-1,C,in),inf(W,X),remainSteps(N). 
possPush(X,gold,W,N,L,C) :- free(L-1,C),piece(W,gold,L,C,in),piece(X,silver,L+1,C,in)|piece(X,silver,L-1,C,in)|piece(X,silver,L,C+1,in)|piece(X,silver,L-1,C,in),inf(W,X),remainSteps(N).
 
%predicat Get_Move, on ajoute un move au tableau
get_moves([[T|Q],[[L1,C1],[L2,C2]]], Gamestate, Board) :- get_moves([T|Q],Gamestate, Board), move([L1,C1],[L2,C2]).

consult(arimaa.pl).

