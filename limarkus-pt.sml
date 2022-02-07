(* Markus Y. Li            *) 
(* SML pattern style       *) 

(* FUNCTION NAME: product                                                                            *)
(* DESCRIPTION  : Computes the product of two integers and also takes care of the negative numbers.  *)	
fun product(_,0) = 0 
|	product(a,b) = if b > 0 then a + product(a,b-1) else ~a + product(a, b + 1); 
	
	
(* FUNCTION NAME: delnthc                                                                            *)
(* DESCRIPTION  : Deletes the n-th character of a string.                                            *)
(* ASSUMPTION   : We can assume that the input string is always longer than n.                       *) 	
fun search(_::xs,1) = xs 
|	search(x::xs,n) = x :: search(xs, n-1); 

fun delnthc(s,n) = 
	implode(search(explode(s), n)); 


(* FUNCTION NAME: <name of the function>       *)
(* DESCRIPTION: <description of the function>  *)
fun search(x::_, 1) = x 
|	search(x::xs, n) = search(xs, n-1); 

fun disnthc(s,n) = search(explode(s), n); 


(* FUNCTION NAME: multin                                                                             *)
(* DESCRIPTION  : Takes a list of three [a,b,c] and multiplies a by b, c times.                      *)
fun multin([_,_,-1]) = nil 
|	multin([a,b,c]) = a:: multin([a * b, b, c-1]);
	

(* FUNCTION NAME: remv                                                                               *)
(* DESCRIPTION  : Removes elements from a list (including all multiple appearance)                   *)
fun remv(_,nil) = nil 
|	remv(n,x::xs) = if x=n then remv(n,xs) else x::remv(n,xs); 


(* FUNCTION NAME: remvdub                                                                            *)
(* DESCRIPTION  : Removes duplicate elements from a list                                             *)
fun remv(_,nil) = nil 
|	remv(n,x::xs) = if x=n then remv(n,xs) else x::remv(n,xs);

fun remvdub(nil) = nil 
|	remvdub(x::xs) = x ::remvdub((remv(x,xs))); 

 
(* FUNCTION NAME: min2                                                                               *)
(* DESCRIPTION  : Computes the second  smallest of number of an integer list.                        *)
(* ASSUMPTION   : The list has at least 2 numbers and all numbers are distinct.                      *) 	
fun remv(_,nil) = nil 
|	remv(n,x::xs) = if x=n then remv(n,xs) else x::remv(n,xs);

fun is_smaller([a]) = a
|	is_smaller(x::y::xs) = if x<y then is_smaller(x::xs) else is_smaller(y::xs); 

fun min2(L) = is_smaller(remv(is_smaller(L), L)); 

(* FUNCTION NAME: int2str                                                                            *)
(* DESCRIPTION  : Converts an integer to a string                                                    *)

fun imp(L) = implode(L); 

fun apnd(nil, y::ys) = y::ys
|	apnd (x::xs, y::ys) = x:: apnd(xs, y::ys);

fun form(0) = nil 
|	form(n) = (n mod 10) :: form(n div 10); 

fun reverse(nil) = nil 
|	reverse(x::xs) = apnd(reverse(xs), [x]); 

fun clist(nil) = nil 
|	clist(x::xs) = chr(48+(x))::clist(xs); 

fun int2str(n) = 
	if n<0 then imp(#"~"::(reverse(clist(form(n-(n*2))))))
		else imp(clist(form(n))); 


(* FUNCTION NAME: pairStar                                                                            *)
(* DESCRIPTION  : Forms a new string where identical chars that are adjacent in the original string   *)
(* 				  are separated from each other by a "*"                                              *)
fun help(nil) = nil 
|	help([a]) = a::nil 
|	help(x::y::xs) = if x=y then x::"*"::help(y::xs) else x:: help(y::xs); 

fun pairStar(s) = implode(help(explode(s))); 
