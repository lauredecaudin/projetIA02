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


inf(X,Y) :- inf1(X,Y).
inf(X,Y) :- inf1(X,Z), inf(Z,Y).

inf1(rabbit, cat).
inf1(cat, dog).
inf1(dog, horse).
inf1(horse, camel).
inf1(camel, elephant).

move([[],[]], L, L).
modèle : 
move([X|L1], L2,[X|X3]) :- move(L1,L2,L3).

/*Petit prédicat pour trouver le joueur adverse*/
oppPlayer(silver, gold).
oppPlayer(gold, silver).


/*Renvoie la couleur d'un pion à partir de son type*/
/*modifier les appelations des pions, mais j'ai trouvé cette façon de faire sur le Git de P16 */
findColour(IdPion, silver) :- element(IdPion, [kr, r1, r2, r3, r4, r5]), !.
findColour(IdPion, gold) :- element(IdPion, [ko, o1, o2, o3, o4, o5]), !.



Une pièce est définie par un tuple pion(IdPion, Col, Lin, Etat), où :

    IdPion est l'identifiant unique de la pièce (rg0, rg1, ... pour les lapins dorés, cg0, cg1... pour les camels dorés etc; rs0, rs1,..., ds0, hs0, ... pour les silvers).
    (Col, Lin) est la position de la pièce sur le plateau. (plateau de 8x8)
    Etat détermine si la pièce est en jeu, si elle est en jeu et est frozen, ou si elle est hors jeu (dans un piège); Etat peut prendre les valeurs 'in', 'frozen' ou 'out'.
    
/* Faut lire le README du Git de P16, ya pleins de prédicats utiles */ 
https://github.com/vincebhx/IA02-Khan/blob/master/README.md


