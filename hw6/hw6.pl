/* hw6.pl */
/*
Name: Frank She
ID: 204172020
People I discussed this with:
Additional Comments:
*/

duplist([], []).
duplist([H|T1], [H,H|T2]) :- duplist(T1, T2).

subseq([],_).
subseq([H|T], [H|T2]) :- subseq(T, T2).
subseq([H1|T1], [H2|T2]) :- H1 \== H2, subseq([H1|T1], T2).



translate([], 0).
translate([H|T], R) :- translate(T, L0), L1 is H, R is 10*L0+L1.
verbalarithmetic(X, Y, Z, W) :- 
	PossibleValues = [1,2,3,4,5,6,7,8,9],
	Y=[H1|_],
	Z=[H2|_],
	W=[H3|_],
	fd_all_different(X),
	fd_domain([H1,H2,H3], PossibleValues), 
	fd_domain(X, [0|PossibleValues]),
	fd_labeling(X),
	reverse(Y, Y2),
	reverse(Z, Z2),
	reverse(W, W2),
	translate(Y2, Y3),
	translate(Z2, Z3),
	translate(W2, W3),
	L is Y3 + Z3,
	L = W3.

/* define some helper functions */

move(world([H|T], S2, S3, none), pickup(H, stack1), world(T, S2, S3, H)). 
move(world(S1, S2, S3, H), putdown(H, stack1), world([H|S1], S2, S3, none)) :- H \= none.

move(world(S1, [H|T], S3, none), pickup(H, stack2), world(S1, T, S3, H)). 
move(world(S1, S2, S3, H), putdown(H, stack2), world(S1, [H|S2], S3, none)) :- H \= none.

move(world(S1, S2, [H|T], none), pickup(H, stack3), world(S1, S2, T, H)). 
move(world(S1, S2, S3, H), putdown(H, stack3), world(S1, S2, [H|S3], none)) :- H \= none.

blocksworld(End, [], End).
blocksworld(Start, [Move|Moves], End) :- 
	move(Start, Move, S), blocksworld(S, Moves, End).