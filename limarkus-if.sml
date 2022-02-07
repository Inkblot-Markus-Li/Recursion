(* Markus Y. Li            *) 
(* SML if-then-else style  *) 

(* FUNCTION NAME: product                                                                            *)
(* DESCRIPTION  : Computes the product of two integers and also handles negative inputs.             *)	
fun product(a,b) = 
	if a = 0 then 0 
	else if a > 0 then b + product(a-1,b)
	else ~b + product(a+1, b); 
	
	
(* FUNCTION NAME: delnthc                                                                            *)
(* DESCRIPTION  : Deletes the n-th character of a string.                                            *)
(* ASSUMPTION   : We can assume that the input string is always longer than n.                       *) 	
fun findpos(L,n) = 
	if n = 1 then tl(L)
	else hd(L)::(findpos(tl(L),n-1)); 

fun delnthc(s,n) = 
	implode(findpos(explode(s),n)); 


(* FUNCTION NAME: dispnthc                                                                           *)
(* DESCRIPTION  : Displays the n-th character of a string.                                           *)
(* ASSUMPTION   : We can assume that the input string is always longer than n.                       *) 	
fun findpos(L,n) = 
	if n=1 then hd(L)
	else (findpos(tl(L), n-1));

fun dispnthc(s,n) = findpos(explode(s),n); 


(* FUNCTION NAME: multin                                                                             *)
(* DESCRIPTION  : Takes a list of three [a,b,c] and multiplies a by b, c times.                      *)
fun multin(L) = 
	if hd(tl(tl(L))) =  ~1 then nil 
	else hd(L) :: multin([hd(L) * hd(tl(L)), hd(tl(L)), (hd(tl(tl(L))))-1]); 
	

(* FUNCTION NAME: remv                                                                               *)
(* DESCRIPTION  : Removes elements from a list (including all multiple appearance)                   *)
fun remv(c,L) = 
	if null L then nil 
	else if hd(L) = c then remv(c, tl(L))
	else hd(L)::remv(c,tl(L));
	

(* FUNCTION NAME: remvdub                                                                            *)
(* DESCRIPTION  : Removes duplicate elements from a list                                             *)
fun remv(c,L : string list) = 
	if null L then nil 
	else if hd(L) = c then remv(c,tl(L))
	else hd(L)::remv(c,tl(L));

fun remvdub(L:string list) = 
	if null L then nil
	else hd(L):: remvdub((remv(hd L, tl L))); 

 
(* FUNCTION NAME: min2                                                                               *)
(* DESCRIPTION  : Computes the second  smallest of number of an integer list.                        *)
(* ASSUMPTION   : The list has at least 2 numbers and all numbers are distinct.                      *) 	
fun remv(c,L) = 
	if null L then nil
	else if hd(L) = c then remv(c,tl(L))
	else hd(L)::remv(c,tl(L));

fun is_smaller(L) = 
	if null (tl L) then hd L 
	else if hd L < hd(tl L) then is_smaller(hd L :: tl(tl L))
	else is_smaller(tl L); 

fun min2(L) = 
	if null L then 0 
	else is_smaller(remv(is_smaller(L),L)); 

min2[1,2,4,5,6];

(* FUNCTION NAME: int2str                                                                            *)
(* DESCRIPTION  : Converts an integer to a string                                                    *)

fun imp(L) = implode(L); 

fun form(n) = 
	if n=0 then nil 
	else (n mod 10)::form(n div 10); 

fun reverse(L) = 
	if null L then nil 
	else apnd(reverse(tl L), [hd L]);

fun apnd(L1, L2) = 
	if null L1 then L2 
	else hd L1 :: apnd(tl L1, L2); 
	
fun clist(L) = 
	if null L then nil 
	else (chr(48+(hd L)))::clist(tl L); 

fun int2str(n) = 
	if n<0 then imp(#"~"::(reverse(clist(form(n-(n*2))))))
	else imp(clist(form(n)));

(* FUNCTION NAME: str2int                                                                            *)
(* DESCRIPTION  : Converts an string to an integer.                                                  *)
fun exp(s) = explode(s); 

fun merge(L) = 
	if null L then nil 
	else ord(hd(L))-48:: merge(tl L); 

fun tenth(n) = 
	if n=1 then 1 
	else 10 * tenth(n-1); 

fun numelem(L) = 
	if null L then 0 
	else 1+ numelem(tl L); 

fun place(L) = 
	if null L then 0 
	else if (hd(L)) = 48 then ~1 * place(tl L)
	else (hd(L))*tenth(numelem(L)) + place(tl L); 

fun str2int (s) = place(merge(exp(s))); 


(* FUNCTION NAME: pairStar                                                                            *)
(* DESCRIPTION  : Forms a new string where identical chars that are adjacent in the original string   *)
(* 				  are separated from each other by a "*"                                              *)
fun pairStar(L) = 
	if null L then nil 
	else if null (tl(L)) then L 
	else if hd(L) = hd(tl(L)) then hd(L) :: "*" :: pairStar(tl L) 
	else hd(L) :: pairStar(tl L); 

fun help(nil) = nil 
|	help([a]) = a:: nil
|	help(x::y::xs)= if x=y then x:: #"*" :: help(y::xs) else x :: help(y::xs); 

fun pairStar(s) = implode(help(explode(s)));


