(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
   *****  This function takes an integer list l and returns the sum of the elements of l.
   		  If the list is empty, return 0.
          Otherwise recursively add head element of the list to the sum of the elements of the rest list 
          until all elements have been added. *****
*) 

let rec sumList l = 
	match l with
	| [] -> 0
	| h::t -> h+sumList(t);;


(* combine : int list * int list -> int list
   ***** This is a helpful function that takes two lists as two arguments and append them together. 
   		 If list a is empty, just return list b.
   		 Otherwise recursively insert the head element of the tail list to list b. *****
*)

let rec combine(a,b) = 
	match a with 
	| [] -> b
	| h::t -> h::combine(t,b);;

(* digitsOfInt : int -> int list 
   ***** This function takes an integer n as an argument and if the integer is positive returns the list of digits of n in the order.
    	 If n is not positive, return empty list.
    	 Otherwise recursively append the list that was created by the first k-1 digits of n to the last digit of n.(Suppose n has k digits) *****
   (see the digits function below for an example of what is expected)
 *)

let rec digitsOfInt n =
	if n <= 0 then []
    else  combine(digitsOfInt(n/10),[n mod 10]);;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** This block calculates the additive persistence and the digital root of n.
	*additivePersistence : int -> int
	This function takes a number as an argument and returns the additive persistence of it.
	If n has only one digit, return 0.
	Otherwise recursively add 1 to the additivePersistence of the sum of digits of n.*

	*digitalRoot : int -> int
	This function takes a number as an argument and returns the digital root of it.
	If n has only one digit, return n itself.
	Otherwise recursively calculate the digital root of the sum of digits of n.*
***** *)

let rec additivePersistence n = 
	if n/10 <= 0 then 0
	else additivePersistence(sumList (digits n))+1;;

let rec digitalRoot n = 
	if n/10 <= 0 then n
	else digitalRoot(sumList (digits n));;

(* listReverse : int list -> int list 
   ***** This function takes a list as an argument and returns a list of the elements of l in the reversed order.
    	 If l is empty list, return empty list.
    	 Otherwise recursively append the head element to the end of the tail list. *****
 *)

let rec listReverse l = 
	match l with 
	| [] -> []
	| h::t -> combine(listReverse t,[h]);;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(* palindrome : string -> bool 
   ***** This function takes a string as an argument and returns true if the string is a palindrome and false if it is not.
    	 If the char list of w in reversed order is the same as the char list of w,
    	 then it is panlidrome and returns true .
    	 Otherwise returns false. *****
 *)

let palindrome w = (listReverse (explode w) = explode w);;

(************** Add Testing Code Here ***************)