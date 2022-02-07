/* Markus Y. Li               */ 
/* Prolog                  */

/* FUNCTION NAME: delnth  */
/* DESCRIPTION: Deletes the n-th element of a list. */
/* NOTES: delnth([1, 2, [3,4], 5], 3, X). → X=[1,2,5] */
	
delnth([_|Xs], 1, Xs). 
delnth([X|Xs], A, ANS) :- A1 is A-1, delnth(Xs,A1, ANS2), ANS = [X|ANS2]. 

/* FUNCTION NAME: dispnth  */
/* DESCRIPTION: Displays the n-th element of a list. */
/* NOTES: dispnth([1, [2, 3], 4, 5], 2, X).→ X=[2,3] */

dispnth([X|_],1,X).
dispnth([X|Xs],A,ANS):- A1 is A-1, dispnth(Xs, A1, ANS2), ANS = ANS2.

/* FUNCTION NAME: remv */
/* DESCRIPTION: Removes elements from a list (including all multiple appearance). */
/* NOTES: remv(a, [a, b, a, c], X). → X=[b, c] */

remv(_, [],[]).
remv(A,[X|Xs],ANS2):- A = X, remv(A,Xs,ANS2).
remv(A,[X|Xs],[X|ANS]):- remv(A,Xs,ANS).

/* FUNCTION NAME: remvdub */
/* DESCRIPTION: Removes duplicate elements from a list. */
/* NOTES: remvdub([a, b, a, c, b, a], X). → X=[a,b,c] */

helper(_,[],[]).
helper(A,[A|Xs], ANS) :- helper(A, Xs, ANS).
helper(A,[X|Xs], [X|ANS]):- helper(A, Xs, ANS).

remvdub([],[]).
remvdub([X|Xs], [X|ANS]) :- helper(X, Xs, ANS2), remvdub(ANS2,ANS).

/* FUNCTION NAME: maxl */
/* DESCRIPTION: Finds the max integer of an integer list. */
/* NOTES: maxl([1, 3, 2, 5, 4], X). → X=5 */

helper(A, [], A).
helper(A, [X|Xs], ANS) :- X > A, helper(X, Xs, ANS). 
helper(A, [X|Xs], ANS) :- helper(A, Xs, ANS).

maxl([],-1).
maxl([X|Xs],ANS):- helper(X,Xs,ANS2), ANS is ANS2.

/* FUNCTION NAME: suml */
/* DESCRIPTION: Returns the sum of an hybrid integer list. */
/* NOTES: suml([1, [2, 3], [4], 5], X).→ X=15 */

suml([],0).
suml([X|Xs],ANS):- atomic(X), suml(Xs, ANS2), ANS is X + ANS2.
suml([X|Xs],ANS):- suml(X, ANS2), suml(Xs, ANS3), ANS is ANS2 + ANS3.

/* FUNCTION NAME: oddths  */
/* DESCRIPTION: Returns a list that contains only the odd-th elements of the given list. */
/* NOTES: oddths([a, b, c, d, e, f, g], X) → X=[a, c, e, g] */

helper(_,[],[]).
helper(A,[X|Xs],[X|ANS]):- 1 is mod(A,2), A1 is A+1,  helper(A1,Xs, ANS).
helper(A,[X|Xs], ANS2):- A1 is A+1, helper(A1,Xs,ANS2).

oddths([X|Xs], A):- helper(1,[X|Xs],A).

/* FUNCTION NAME: inde  */
/* DESCRIPTION: Returns the index (start from 1) of the occurrence of a given value. */
/* NOTES: inde(1, [1, 2, 1, 1, 2, 2, 1], X). → X=[1, 3, 4, 7] */

helper(_,[],_,[]).
helper(A,[X|Xs],B,[B|ANS]):- A=X, B1 is B+1, helper(A,Xs,B1,ANS).
helper(A,[X|Xs],B,ANS):- B1 is B+1, helper(A,Xs,B1,ANS).

inde(M,[X|Xs],ANS):- helper(M,[X|Xs],1,ANS).

/* FUNCTION NAME: nele  */
/* DESCRIPTION: Repeats each element in a list n times. */
/* NOTES: nele([1, 3, 5], 3, X).→ X=[1, 1, 1, 3, 3, 3, 5, 5, 5] */

helper([],_,_,[]).
helper([X|Xs],A,B,ANS):- B=<A, B1 is B+1, helper([X|Xs],A,B1,ANS2), ANS=[X|ANS2].
helper([X|Xs],A,B,ANS):- helper(Xs,A,1,ANS).

nele([X|Xs],A,ANS):- helper([X|Xs],A,1,ANS2), ANS = ANS2.

/* FUNCTION NAME: primeton  */
/* DESCRIPTION: Finds all prime numbers from 2 to a given number n. */
/* NOTES: primeton(20, X). → X=[2,3,5,7,11,13,17,19] */

helper1(A, B) :- 0 < A mod B.
helper2(A, B) :- A = B.
helper2(A, B) :- B1 is B+1, helper1(A, B), helper2(A,B1).
helper3(A, A, C, []).
helper3(A, B, C, [A|Ans]) :- A1 is A+1, helper2(A, C), helper3(A1, B, C, Ans).
helper3(A, B, C, Ans) :- A1 is A+1, helper3(A1, B, C, Ans).

primeton(A, Ans) :- helper3(2, A, 2, Ans).